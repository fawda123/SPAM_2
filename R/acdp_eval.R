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
#   alpha <- atan(evecs[ , 1][2]/evecs[ , 1][1])
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

######
# bin average

library(dplyr)
library(tidyr)
library(ggplot2)

data(adcp_dat)
data(pbay)

dat_in <- adcp_dat

datN <- vecrots(adcp_dat)
datE <- vecrots(adcp_dat, 90)
# to use 1:3 based on exploratory plots above
bins <- 1:3

# get north/east components of bins 1:3, then average
datN <- vecrots(dat_in) %>% 
  .[, grep(paste(c('datetimestamp', bins), collapse = '|'), names(.))] %>% 
  .[, grep('datetimestamp|Mag', names(.))] %>% 
  mutate(MagN = rowMeans(.[grepl('Mag', names(.))], na.rm = TRUE)) %>% 
  select(datetimestamp, MagN)

datE <- vecrots(dat_in, 90) %>% 
  .[, grep(paste(c('datetimestamp', bins), collapse = '|'), names(.))] %>% 
  .[, grep('datetimestamp|Mag', names(.))] %>% 
  mutate(MagE = rowMeans(.[grepl('Mag', names(.))], na.rm = TRUE)) %>% 
  select(datetimestamp, MagE)

# combine, recreate vector from averaged N/S vecs
dat <- full_join(datN, datE, by = 'datetimestamp')
dat$dir <- with(dat, atan2(MagE, MagN) * 180/pi)
dat$dir[dat$dir < 0] <- 360 - abs(dat$dir[dat$dir < 0])
dat$mag <- with(dat, MagE / sin(pi * dir / 180))

# # compare with original bins
# toplo <- adcp_dat[, c('datetimestamp', 'Mag1', 'Dir1', 'Mag2', 'Dir2', 'Mag3', 'Dir3')]
# toplo$Mag4 <- dat$mag # fourth bin is the averaged of the first three
# toplo$Dir4 <- dat$dir
# 
# pdf('C:/Users/mbeck/Desktop/agg_tide.pdf', height = 8, width =  3)
# for(i in 1:100){
#   cat(i, '\t')
#   p <- plot_adcpraw(toplo[i, ], shp_in = pbay, bins = 1:4,
#     loc_in = c(-87.13208, 30.45682), vec_scl = 2)
#   print(p)
# }
# dev.off()

# get principal component of the combined vector
eig <- eigen(cov(na.omit(dat[, c('MagN', 'MagE')])))
vecs <- eig$vectors

dirP <- atan(vecs[2, 1]/vecs[1, 1]) * 180/pi
if(dirP < 0) dirP <- 360 - abs(dirP)
dat$dirP <- dirP

# get magnitude along primary axis (use original vector)
diffval <- dat$dir - dat$dirP
dat$magsP <- dat$mag * cos(pi * diffval/180)

# # make magnitute abs, if negative add 180 to principal vec
# dat$dirP[dat$magsP < 0] <- dat$dirP[dat$magsP < 0] + 180
# dat$magsP <- abs(dat$magsP)

# # compare with original bins
# toplo <- adcp_dat[, c('datetimestamp', 'Mag1', 'Dir1', 'Mag2', 'Dir2', 'Mag3', 'Dir3')]
# toplo$Mag4 <- dat$mag # fourth bin is the averaged of the first three
# toplo$Dir4 <- dat$dir
# toplo$Mag5 <- dat$magsP # fifth bin is the vector along the principal axis
# toplo$Dir5 <- dat$dirP
#
# labs <- c('Bin 1: 2.5m', 'Bin 2: 2 m', 'Bin 3: 1.5 m', 'Averaged', 'PC axis')
# pdf('C:/Users/mbeck/Desktop/agg_tide.pdf', height = 8, width =  3)
# for(i in 30:35){
#   cat(i, '\t')
#   p <- plot_adcpraw(toplo[i, ], shp_in = pbay, bins = 1:5, bin_labs = labs,
#     loc_in = c(-87.13208, 30.45682), vec_scl = 2)
#   print(p)
# }
# dev.off()



