# load data
load("../data/R0_curves/malaria_R0.Rsave")
Aedes.R0.out = read.csv("../data/R0_curves/AedesR0Out.csv", header = T)

# combine data
R0_curves <- cbind(Aedes.R0.out, "anopheles.R0.median" = malaria / max(malaria))

# format
R0_curves$MalariaToDengueRatio <- R0_curves$anopheles.R0.median / R0_curves$aegypti.R0.median
# R0_curves$MalariaToDengueRatio[is.nan(R0_curves$MalariaToDengueRatio)] <- 0

m = 0.2
d = 0.4
(m/d)
d/m

pdf("../figures/R0/malaria_vs_dengue.pdf", height = 5, width = 9)

plot(R0_curves$temperature,
     R0_curves$MalariaToDengueRatio, 
     xlim = c(12, 37),
     lwd = 2, 
     type = "l", 
     col = "black", 
     xlab="", 
     ylab="")

abline(h = 1, lty = 3)

rect(xleft = 27, 
     xright = 35.2, 
     ybottom = par("usr")[3], 
     ytop = par("usr")[4], 
     border = 'black', 
     col = adjustcolor("blue", alpha = 0.3))


rect(xleft = 15.5, 
     xright = 17.5, 
     ybottom = par("usr")[3], 
     ytop = par("usr")[4], 
     border = 'black', 
     col = adjustcolor("blue", alpha = 0.3))


text(x = 14.5,
     y = 0.6,
     "Dengue exceeds\nmalaria\n(15.5, 17.5C)")

text(x = 21.6,
     y = 2,
     "Malaria exceeds\ndengue\n(17.6, 27C)")

text(x = 31.5,
     y = 0.6,
     "Dengue exceeds\nmalaria\n(27.1, 35.2C)")


dev.off()

library(raster)
library(ncdf4)

# x <- brick("data/climate/air.2020.nc")
x <- raster("data/climate/worldclim/wc2.1_30s_tavg_01.tif")
x2 <- values(x)

# possible soluation: https://rdrr.io/cran/raster/man/subs.html
x[x < 0] <- 0

plot(x)
