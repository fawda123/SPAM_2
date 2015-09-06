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

######
# match ctd dates with flow

data(ctd_dat)
data(flo_dat)
data(wqm_dat)

library(CTDplot)
library(ggplot2)
library(SWMPr)
library(dplyr)
library(gridExtra)
library(tidyr)
source('R/funcs.R')

# split ctd by dates, get unique dates
ctd <- split(ctd_dat, ctd_dat$Date)

# date ranges from wqm
rngs <- as.Date(range(wqm_dat$datetimestamp, na.rm = T))

# plot of ten day average of flow, colored by quantiles
# also shows dates of ctd casts
flo_dat <- filter(flo_dat, rngs[1] <= Date & Date <= rngs[2])
flo_dat$discharge <- smoother(flo_dat, window = 10, sides = 1)[, 2]
levs <- quantile(flo_dat$discharge, c(0.33, 0.66), na.rm = TRUE)
flo_dat$levels <- cut(flo_dat$discharge, breaks = c(-Inf, levs, Inf), labels = c('low', 'med', 'hi'))
uni_dts <- unique(ctd_dat$Date)
flo_dat <- filter(flo_dat, rngs[1] <= Date & Date <= rngs[2])

p1 <- ggplot(flo_dat, aes(x = Date, y = discharge, colour = levels)) + 
  geom_line(aes(group = 1), size = 1) + 
  scale_y_continuous('10 day mean disharge (m3/s)') + 
  scale_colour_manual(values = RColorBrewer::brewer.pal(9, 'Set1')[c(1:3)]) + 
  geom_vline(xintercept = as.numeric(uni_dts), linetype = 'dashed') + 
  theme_classic() + 
  theme(axis.title.x = element_blank(), legend.position = 'top', legend.title = element_blank())

# plot of DO at P05-B
# also shows dates of ctd casts
do_dat <- filter(wqm_dat, stat %in% 'P05-B') %>% 
  select(datetimestamp, do_mgl) %>% 
  mutate(do_daily = smoother(do_mgl, window = 48)[, 1])
do_dat$cols <- cut(do_dat$do_daily, c(-Inf, 2, Inf), labels = c('Hypoxic', 'Normoxic'))

p2 <- ggplot(do_dat, aes(x = datetimestamp, y = do_daily, colour = cols)) + 
  geom_line(aes(group = 1), size = 1) + 
  scale_y_continuous('daily mean DO (mg/L)') + 
  scale_colour_manual(values = c('black', 'grey')) + 
  geom_vline(xintercept = as.numeric(as.POSIXct(uni_dts)), linetype = 'dashed') + 
  theme_classic() + 
  theme(axis.title.x = element_blank(), legend.position = 'top', legend.title = element_blank())

##
# get bottom DO at each station for all dates in ctd data
# make in grid format of dist by date for interp
do_mat <- select(ctd_dat, Station, Date, Depth, DO, dist) %>% 
  group_by(Station, Date) %>% 
  mutate(maxd = max(Depth, na.rm = T)) %>% 
  filter(Depth == maxd) %>%
  ungroup %>% 
  select(Date, dist, DO) %>% 
  spread(Date, DO) %>% 
  data.frame
  
# interp for plot

# first create new grid
uni_dts <- sort(unique(ctd_dat$Date))
dists <- unique(ctd_dat$dist)
num_int <- 200
new_grd <- expand.grid(
    approx(dists, n = num_int)$y, 
    approx(uni_dts, n = num_int)$y
    )

# then interp
int_val <- fields::interp.surface(
  obj = list(  
    x = dists, 
    y = uni_dts, 
    z = do_mat[,-1]), 
  loc = new_grd
  )
do_mat <- cbind(new_grd, int_val)
names(do_mat) <- c('Distance', 'Date', 'DO')
do_mat <- spread(do_mat, Date, DO)
x.val <- as.numeric(names(do_mat)[-1])
y.val <- do_mat$Distance
z.val <- as.matrix(do_mat[, -1])
cols <- RColorBrewer::brewer.pal(9, 'Set1')[c(1:3)]
in_col <- colorRampPalette(cols)
rotate <- function(x) t(apply(x, 2, rev))

# contour plot with isolines
filled.contour3(x = x.val, y = y.val, z = t(z.val),
  color.palette = in_col,
  nlevels = 8, # for smoothed colors
  axes = F)
contour(x = x.val, y = y.val, z = t(z.val), nlevels= 8,
  axes = F, add = T)
# axis labels
axis(side = 2)
axis.Date(side = 3, x = as.Date(x.val), format = '%m-%Y')
axis.Date(side = 1, x = as.Date(x.val), at = uni_dts, labels = F, lwd.ticks = 2, col = 'grey')
axis(side = 4, at = unique(ctd_dat$dist), labels = unique(ctd_dat$Station), tick = F, col = 'grey', cex.axis = 0.6, las = 1, hadj = 1)
box()

grid.arrange(p1, p2, ncol = 1)

# selected dates
# low, med, high flow
# 2014-10-15, 2015-04-29, 204-05-13 (8, 14, 2)
sel_dts <- as.character(uni_dts[c(8, 14, 2)])
sel <- names(ctd) %in% sel_dts
ctd_plotmult(ctd[sel_dts], var_plo = 'Salinity')
ctd_plotmult(ctd[sel_dts], var_plo = 'DO')
