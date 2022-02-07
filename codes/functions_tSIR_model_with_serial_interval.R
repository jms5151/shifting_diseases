# functions to rub tSIR model with serial interval for effective 
# infectious individuals

calculate_serial_interval_weights <- function(serial_interval_mean, serial_interval_sd){
  shape = (serial_interval_mean / serial_interval_sd)^2
  rate = serial_interval_mean / (serial_interval_sd^2)
  
  gamma_dist <- function(t){
    ( pgamma(t + 7, shape, rate) - pgamma(t, shape, rate) ) / 7
  }
  
  times <- (0:6)*7
  
  weights <- rep(NA, 5)
  
  for(i in 1:5){
    weights[i] <- stats::integrate(f = gamma_dist
                                   , upper = times[i]
                                   , lower = times[i+1])$value / 7
  }
  
  weights / sum(weights)  
}

tsir_with_serial_interval <- function(df, weights){
  cumulative_cases <- cumsum(df$cases)
  cumulative_births <- cumsum(df$births)
  alpha_value <- 0.74
  
  mod1 <- lm(cumulative_births ~ cumulative_cases)
  Yhat_cum_cases <- predict(mod1)
  
  rho <- derivative(cumulative_cases, Yhat_cum_cases)
  
  Zt <- resid(mod1) 
  
  # adjust infected class based on reporting rate, rho
  Iadjusted <- df$cases + (rho * df$cases)
  
  # calculate effective infectious individuals
  Ieffective <- rep(NA, length(Iadjusted - 5))
  
  for(j in 1:length(Ieffective)){
    Ieffective[j] <- sum(
      Iadjusted[(j+4):j] * weights, na.rm=T)
  }
  
  # period <- rep(1:(52/2), each = 2, round(nrow(df) + 1))[1:(nrow(df) - 5)]
  # my change to get a beta for every week:
  period <- seq(1, nrow(df) - 5, by = 1)
  
  # since linear regression is includes St-1, offset variables appropriately
  # may have too offset by 6 weeks because of serial interval
  Iadjusted <- Iadjusted[6:length(df$cases)]
  Iadjusted <- Iadjusted + 0.00001 # add a small amount so there are no true zeros
  Ieffective <- Ieffective[6:length(df$cases)]
  N <- df$pop[6:length(df$cases)]
  Ztminus1 <- Zt[5:(length(df$cases)-1)]
  alpha <- rep(alpha_value, length(df$cases)-5)
  
  # set range for profile likelihood with Sbar
  # this is directly from tsiR package
  min_S_bar <- max(0.01 * N, -(min(Ztminus1) - 1)) 
  max_S_bar <- 0.4 * mean(N)
  S_bar_gradient <-  seq(min_S_bar, max_S_bar, length = 250) # 250 is slow
  loglik <- rep(NA, length(S_bar_gradient))
  
  link <- "identity"
  family <- 'gaussian'
  
  # If using offsets for "known" slopes (log(N) - end of equation), 
  # betas are much smaller, it also reduces number of betas by 5 * 3 offsets
  for (i in 1:length(S_bar_gradient)) {
    glmfit <- glm(
      log(Iadjusted) ~ -1 +
        as.factor(period) +
        offset(log(N)) +
        offset(log(S_bar_gradient[i] + Ztminus1)) -
        offset(alpha * log(Ieffective)),
      family = eval(parse(text = family))(link = link)
    )
    loglik[i] <- glmfit$deviance
  }
  
  S_bar <- S_bar_gradient[which.min(loglik)]
  
  glmfit <- glm(
    log(Iadjusted) ~ -1 +
      as.factor(period) +
      offset(log(N)) +
      offset(log(S_bar + Ztminus1)) -
      offset(alpha * log(Ieffective)),
    family = eval(parse(text = family))(link = link)
  )
  
  betas <- exp(coef(glmfit)) # exponentiated bc values in models are logged
  
  return(
    list(
      "Year" = df$time
      , "WOY" = df$WOY
      , "S_ba" = S_bar
      , "Ieffective" = Ieffective
      , "Iadjusted" = Iadjusted
      , "betas" = betas
    )
  )
}
