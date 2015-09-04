######
# adcp exploring, Sep. 2015

# ######
# # plot the eigenvectors 
# data(adcp_datP)
# 
# dat <- split(adcp_datP, adcp_datP$bin)
# eigs <- lapply(dat, function(x){
#   mats <- cov(na.omit(x[, c('magsN', 'magsE')]))
#   eigen(mats)
#   }) 
# 
# par(mfrow = c(2, 4))
# 
# for(i in 1:8){
#   
#   toplo <- data.frame(dat[[i]][, c('magsN', 'magsE')])
#   
#   eigens <- eigs[[i]]
#   evecs <- eigens$vectors
#   evs <- sqrt(eigens$values)
#   
#   a <- evs[1]
#   b <- evs[2]
#   x0 <- 0
#   y0 <- 0
#   
#   alpha <- atan(evecs[ , 1][2] / evecs[ , 1][1])
#   theta <- seq(0, 2 * pi, length=(1000))
#   
#   x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
#   y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
#   
#   plot(magsN ~ magsE, data = toplo, asp = 1, main = paste0('Bin ', i))
#   lines(y, x, col = 'green')
#   abline(0, evecs[, 1][1]/evecs[, 1][2], col = 'blue')
#   abline(0, evecs[, 2][1]/evecs[, 2][2], col = 'blue')
#   segments(0, 0, -1* a * evecs[ , 1][2], -1 * a * evecs[ , 1][1], col = 'red')
#   segments(0, 0, b * evecs[ , 2][2], b * evecs[ , 2][1], col = 'red')
# 
# }
# 
# ######
# # plot the cumulative distances along the principal vector
# 
# data(adcp_datP)
# 
# dists <- vecdist(adcp_datP)
# 
# ggplot(dists, aes(x = datetimestamp, y = cDisP)) +
#   geom_point(size = 0.5) + 
#   theme_bw() + 
#   facet_wrap(~ bin, ncol = 1)
  