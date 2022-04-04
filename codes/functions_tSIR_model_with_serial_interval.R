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

# df = brazil_dengue_tsir_data[[15]]

tsir_with_serial_interval <- function(df, weights){
  # do this at the beginning
  # calculate effective infectious individuals
  Ieffective <- rep(NA, length(df$cases) - 5)

  for(j in 1:length(Ieffective)){
    Ieffective[j] <- sum(
      df$cases[(j+4):j] * weights, na.rm=T
    )
  }
  
  cumulative_cases <- cumsum(Ieffective) # X
  cumulative_births <- cumsum(df$births[6:length(df$births)]) # Y
  alpha_value <- 0.74
  
  # see what I'm trying to model 
  # plot(cumulative_cases, cumulative_births, xlim = c(0,50000), ylim = c(0,50000))
  
  mod1 <- lm(cumulative_births ~ cumulative_cases)
  Yhat_cum_cases <- predict(mod1)
  
  # see how the model fit looks
  # plot(cumulative_cases, Yhat_cum_cases, xlim = c(0,50000), ylim = c(0,50000))
  # abline(mod1)

  rho <- derivative(cumulative_cases, Yhat_cum_cases)
  
  while(mean(rho) >= 1){
    cumulative_births <- cumulative_births * 0.30
    mod1 <- lm(cumulative_births ~ cumulative_cases)
    Yhat_cum_cases <- predict(mod1)
    rho <- derivative(cumulative_cases, Yhat_cum_cases)
  }
  # signif(rho, 3)
  # Zt <- resid(mod1)
  Zt <- residual.cases(Yhat_cum_cases, cumulative_cases)
  
  Iadjusted <- Ieffective + (rho * Ieffective)
  
  period <- seq(1, length(Ieffective)-1, by = 1)
  
  # since linear regression is includes St-1, offset variables appropriately
  Iadjusted <- Iadjusted[1:length(Iadjusted)-1]
  Iadjusted <- Iadjusted + 1 # add a small amount so there are no true zeros
  Ieffective <- Ieffective[1:length(Ieffective)-1]
  Ieffective <- Ieffective + 1 # add a small amount so there are no true zeros
  N <- df$pop[7:length(df$cases)]
  Ztminus1 <- Zt[2:length(Zt)]
  alpha <- rep(alpha_value, length(Iadjusted))
  
  # set range for profile likelihood with Sbar
  # this is directly from tsiR package
  min_S_bar <- max(0.08 * N, - (min(Ztminus1) - 1)) # in tsiR package it's 0.01 instead of 0.08
  max_S_bar <- 0.4 * mean(N)
  S_bar_gradient <- seq(min_S_bar, max_S_bar, length = 5) # 250 is slow
  loglik <- rep(NA, length(S_bar_gradient))
  
  link <- "identity"
  family <- 'gaussian'
  
  # Offsets used for "known" slopes 
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
      , "S_bar" = S_bar
      , "S" = S_bar + Ztminus1
      , "rho" = rho
      , "Ieffective" = Ieffective
      , "Iadjusted" = Iadjusted
      , "betas" = betas
    )
  )
}

# following Zika paper, but doesn't seem to work well with low case numbers from
# Kenya
simple_tsir_with_serial_interval <- function(df, weights){
  Ieff <- rep(NA, length(df$cases) - 5)
  
  for(j in 1:length(Ieff)){
    len <- length(df$cases)
    
    Ieff[j] <- sum(
      df$cases[(j+4):j] * weights, na.rm=T
    )
  }
  
  I <- df$cases[6:len]
  sumI <- cumsum(df$cases)
  S <- unique(df$Population) - sumI[5:(len-1)]
  
  # cumulative_cases <- cumsum(Ieffective)
  # cumulative_births <- cumsum(df$births[6:length(df$births)])
  # alpha_value <- 0.74
  
  N <- rep(unique(df$Population), length.out = length(I))

  # logged beta
  betas <- ifelse(Ieff == 0,
                  -Inf,
                  log(I) - log(S) + log(N) - (.74) * log(Ieff))
  
  betas <- ifelse(is.infinite(betas), 0, betas)
  
  return(
    list(
      "Year" = df$Year
      , "WOY" = df$WOY
      , "S" = S
      , "Ieffective" = Ieff
      , "I" = I
      , "betas" = betas
    )
  )

}
