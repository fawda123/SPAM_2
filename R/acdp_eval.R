######
# adcp exploring, Sep. 2015

######
# plot the eigenvectors 
data(adcp_datP)

dat <- split(adcp_datP, adcp_datP$bin)
eigs <- lapply(dat, function(x){
  mats <- cov(na.omit(x[, c('magsN', 'magsE')]))
  eigen(mats)
  }) 

pdf('figs/bin_eigs.pdf', height = 5, width = 9, family = 'serif')
par(mfrow = c(2, 4))

for(i in 1:8){
  
  toplo <- data.frame(dat[[i]][, c('magsN', 'magsE')])
  
  eigens <- eigs[[i]]
  evecs <- eigens$vectors
  evs <- sqrt(eigens$values)
  
  a <- evs[1]
  b <- evs[2]
  x0 <- 0
  y0 <- 0
  
  alpha <- atan(evecs[ , 1][2]/evecs[ , 1][1])
  theta <- seq(0, 2 * pi, length=(1000))
  
  x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
  y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
  
  plot(magsN ~ magsE, data = toplo, asp = 1, main = paste0('Bin ', i))
  lines(y, x, col = 'green')
  abline(0, evecs[, 1][1]/evecs[, 1][2], col = 'blue')
  abline(0, evecs[, 2][1]/evecs[, 2][2], col = 'blue')
  segments(0, 0, -1* a * evecs[ , 1][2], -1 * a * evecs[ , 1][1], col = 'red')
  segments(0, 0, b * evecs[ , 2][2], b * evecs[ , 2][1], col = 'red')

}
dev.off()

######
# plot the cumulative distances along the principal vector

data(adcp_datP)

dists <- vecdist(adcp_datP)

p1 <- ggplot(dists, aes(x = datetimestamp, y = cDisP)) +
  geom_point(size = 0.5) + 
  theme_bw() + 
  facet_wrap(~ bin, ncol = 1)

pdf('figs/cum_dist.pdf', height = 8, width = 7, family = 'serif')
print(p1)
dev.off()

######
# bin average

library(dplyr)
library(tidyr)
library(ggplot2)

data(adcp_dat)
data(adcp_datP)
data(pbay)

# compare with averaged lower 3 and princomp to original bins
toplo <- adcp_dat[, c('datetimestamp', 'Mag1', 'Dir1', 'Mag2', 'Dir2', 'Mag3', 'Dir3')]
toplo$Mag4 <- adcp_datP$Mag # fourth bin is the averaged of the first three
toplo$Dir4 <- adcp_datP$Dir
toplo$Mag5 <- adcp_datP$MagP # fifth bin is the vector along the principal axis
toplo$Dir5 <- adcp_datP$DirP

# plots of bins 1/3, averaged, and decomp along PC axis for multiple timesteps
labs <- c('Bin 1: 2.5m', 'Bin 2: 2 m', 'Bin 3: 1.5 m', 'Averaged', 'PC axis')
pdf('figs/binagg.pdf', height = 8, width =  3)
for(i in 100:200){
  cat(i, '\t')
  p <- plot_adcpraw(toplo[i, ], shp_in = pbay, bins = 1:5, bin_labs = labs,
    loc_in = c(-87.13208, 30.45682), vec_scl = 2)
  print(p)
}
dev.off()

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
  scale_y_continuous('10 day mean discharge (m3/s)') + 
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

# selected dates
# low, med, high flow
# 2014-10-15, 204-07-23, 2015-04-29,  (8, 5, 14)

# split ctd by dates, get unique dates
ctd <- split(ctd_dat, ctd_dat$Date)

sel_dts <- names(ctd)[c(8, 5, 14)]
sel <- names(ctd) %in% sel_dts
flos <- c('Lo flow', 'Med flow', 'Hi flow')

##
# # date by distance hypoxia contour plot with p05 bottom do and flow
# pdf('figs/hypox_plots.pdf', height = 5, width = 10, family = 'serif')
# ctd_bott(ctd_dat, num_levs = 10, ncol = 10)
# grid.arrange(p1, p2, ncol = 1)
# dev.off()

##
# ctd distance by depth plots for salinity/do in three diff flow regimes
pdf('figs/ctd_flos.pdf', height = 7, width = 7, family = 'serif')
ctd_plotmult(ctd[sel_dts], ncol = 8, var_plo = 'Salinity', 
  var_labs = paste0('Salinity ', sel_dts, ', ', flos))
ctd_plotmult(ctd[sel_dts], ncol = 8, var_plo = 'DO', 
  var_labs = paste0('DO ', sel_dts, ', ', flos))
dev.off()

######
# now compare ctd eigen vectors from binned data with bottom water DO at p05 during hypoxia and not during hypoxia
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(SWMPr)
source('R/funcs.R')

data(wqm_dat)
data(adcp_datP)

# date to center eval, plus/minus x * weeks
dt_cent <- as.POSIXct('2014-07-23', tz = 'America/Regina')
dt_cent <- c(dt_cent - 2 * 604800, dt_cent + 2 * 604800)

# subset each by dates
do_dat <- filter(wqm_dat, stat %in% 'P05-B') %>% 
  select(datetimestamp, do_mgl) %>% 
  filter(datetimestamp >= dt_cent[1] & datetimestamp <= dt_cent[2]) 

mv_dat <- filter(adcp_datP, datetimestamp >= dt_cent[1] & datetimestamp <= dt_cent[2]) %>% 
  select(-MagN, -MagE)

# combine, get differences
# dist is the approximate distance travelled by a parcel at time t2 for the preceding two hours based on an average of speed at t1 and t2 multipled by two hours, then converted to km
# 
cum.na <- function(x) { 
  x[which(is.na(x))] <- 0 
  return(cumsum(x)) 
} 

dat <- comb(do_dat, mv_dat, date_col = 'datetimestamp', timestep = 120) %>% 
  mutate(
    dist = smoother(MagP, 2)[, 1] *  60 * 60 * 2 / 1000, 
    cumdist = cum.na(dist),
    dxdt = c(NA, diff(dist)), 
    ddodt = c(NA, diff(do_mgl))
  )

pdf('figs/JulyDO.pdf', family = 'serif', height = 5, width = 6)
par(mfrow = c(3, 2), mar = c(4.5, 4.5, 0.5, 0.5))
plot(do_mgl ~ datetimestamp, data = dat, type = 'l')
plot(ddodt ~ datetimestamp, data = dat, type = 'l')
plot(cumdist ~ datetimestamp, data = dat, type = 'l')
plot(do_mgl ~ MagP, data = dat)
plot(ddodt ~ dxdt, data = dat)
mod <- lm(ddodt ~ dxdt, data = dat)
abline(reg = mod)
dev.off()

######
# look at moving window corrs/regs of ddodt by dxdt

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(SWMPr)
source('R/funcs.R')

data(wqm_dat)
data(adcp_datP)

do_dat <- filter(wqm_dat, stat %in% 'P05-B') %>% 
  select(datetimestamp, do_mgl)

mv_dat <- select(adcp_datP, -MagN, -MagE)

## 
cum.na <- function(x) { 
  x[which(is.na(x))] <- 0 
  return(cumsum(x)) 
} 

dat <- comb(do_dat, mv_dat, date_col = 'datetimestamp', timestep = 120, method = 2) %>% 
  mutate(
    dist = smoother(MagP, 2)[, 1] *  60 * 60 * 2 / 1000, 
    cumdist = cum.na(dist),
    dxdt = c(NA, diff(dist)), 
    ddodt = c(NA, diff(do_mgl))
  )

winsz <- 1 * 336 # approx one month (step is two hours)

slo <- rep(NA, length = nrow(dat))
rsq <- corr <- slo
for(i in 1:nrow(dat)){
  
  cat(i, '\t')
  
  tomod <- na.omit(dat[c(i:(i + winsz - 1)), c('dxdt', 'ddodt')])
  if(nrow(tomod) == 0) next
  
  mod <- with(tomod, lm(ddodt ~ dxdt, tomod))
  sloi <- coef(mod)[2]
  rsqi <- summary(mod)$r.squared
  corri <- with(tomod, cor(dxdt, ddodt))
  corr[i] <- corri
  slo[i] <- sloi
  rsq[i] <- rsqi
  
}

wins <- 1:2000
pdf('figs/mw_eval.pdf', height = 5, width = 6, family = 'serif')
par(mfrow = c(3, 1), mar = c(4.5, 4.5, 0.5, 0.5))
plot(dat$datetimestamp[wins], corr[wins], type = 'l')
plot(dat$datetimestamp[wins], slo[wins], type = 'l')
plot(dat$datetimestamp[wins], rsq[wins], type = 'l')
dev.off()
dat$datetimestamp[which.min(corr[wins])]

######
## now compare ctd eigen vectors from binned data with bottom water DO at p05 during hypoxia and not during hypoxia
# rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(SWMPr)
source('R/funcs.R')

data(wqm_dat)
data(adcp_datP)

# date to center eval, plus/minus x * weeks
dt_cent <- as.POSIXct('2014-09-03', tz = 'America/Regina')
dt_cent <- c(dt_cent, dt_cent + 4 * 604800)

# subset each by dates
do_dat <- filter(wqm_dat, stat %in% 'P05-B') %>% 
  select(datetimestamp, do_mgl) %>% 
  filter(datetimestamp >= dt_cent[1] & datetimestamp <= dt_cent[2]) 

mv_dat <- filter(adcp_datP, datetimestamp >= dt_cent[1] & datetimestamp <= dt_cent[2]) %>% 
  select(-MagN, -MagE)

# combine, get differences
# dist is the approximate distance travelled by a parcel at time t2 for the preceding two hours based on an average of speed at t1 and t2 multipled by two hours, then converted to km
# 
cum.na <- function(x) { 
  x[which(is.na(x))] <- 0 
  return(cumsum(x)) 
} 

dat <- comb(do_dat, mv_dat, date_col = 'datetimestamp', timestep = 120) %>% 
  mutate(
    dist = smoother(MagP, 2)[, 1] *  60 * 60 * 2 / 1000, 
    cumdist = cum.na(dist),
    dxdt = c(NA, diff(dist)), 
    ddodt = c(NA, diff(do_mgl))
  ) %>% 
  na.omit

pdf('figs/SepDO.pdf', family = 'serif', height = 5, width = 6)
par(mfrow = c(3, 2), mar = c(4.5, 4.5, 0.5, 0.5))
plot(do_mgl ~ datetimestamp, data = dat, type = 'l')
plot(ddodt ~ datetimestamp, data = dat, type = 'l')
plot(cumdist ~ datetimestamp, data = dat, type = 'l')
plot(do_mgl ~ MagP, data = dat)
plot(ddodt ~ dxdt, data = dat)
mod <- lm(ddodt ~ dxdt, data = dat)
abline(reg = mod)
dev.off()

