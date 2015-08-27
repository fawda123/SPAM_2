# source('R/funcs.R')
# 
# library(dplyr)
# library(tidyr)
# library(SWMPr)
# 
# data(adcp_dat)
# data(pbay)
# 
# 
# dat_in <- adcp_dat
# 
# # bins to decompose (all)
# bins <- length(grep('^Dir', names(dat_in)))
# bins <- c(1:bins)
#   
# # subset directions, mags by bins
# dirs <- dat_in[, grep(paste(paste0('^Dir', bins), collapse = '|'), names(dat_in))] %>% 
#   data.frame(datetimestamp = dat_in$datetimestamp, .) %>% 
#   gather('variable', 'value', -datetimestamp)
# mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))] %>% 
#   data.frame(datetimestamp = dat_in$datetimestamp, .) %>% 
#   gather('variable', 'value', -datetimestamp)
# 
# # get diff of observed data from rotation angle
# diffN <- dirs$value - 360
# diffE <- dirs$value - 90
#   
# # get magnitude of new vectors
# magsN <- mags$value * cos(pi * diffN/180)
# magsE <- mags$value * cos(pi * diffE/180)
# 
# # 
# mags <- data.frame(mags, magsN, magsE) %>% 
#   mutate(bin = gsub('^Mag', 'Bin', variable)) %>% 
#   select(-variable) %>% 
#   na.omit
# 
# mags <- split(mags, mags$bin)
eigs <- lapply(mags, function(x){
  eigen(cov(x[, c('magsN', 'magsE')]))
  }) 

######

toplo <- data.frame(mags[[2]][, c('magsN', 'magsE')])

eigens <- eigs[[2]]
evecs <- eigens$vectors
evs <- sqrt(eigens$values)

a <- evs[1]
b <- evs[2]
x0 <- 0
y0 <- 0

alpha <- atan(evecs[ , 1][2] / evecs[ , 1][1])
theta <- seq(0, 2 * pi, length=(1000))

x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)

plot(magsN ~ magsE, data = toplo, asp = 1)
lines(y, x, col = 'green')
abline(0, evecs[, 1][1]/evecs[, 1][2])
abline(0, evecs[, 2][1]/evecs[, 2][2])
arrows(0, 0, a * evecs[ , 1][2], a * evecs[ , 1][1], col = 'red')
arrows(0, 0, b * evecs[ , 2][2], b * evecs[ , 2][1], col = 'red')

abline(reg = lm(magsN ~ 0 + magsE, data = toplo), lty = 2, col = 'yellow', lwd = 2)
mod2 <- lm(magsE ~ 0 + magsN, data = toplo)
xval <- seq(-4, 4, length = 1000)
yval <- (xval)/coef(mod2)[1]
lines(xval, yval, col = 'green', lty = 2, lwd =2)