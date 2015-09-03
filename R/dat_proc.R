######
# SPAM 2 processing of raw data

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
# devtools::load_all('M:/docs/SWMPr')
library(SWMPr)
library(WtRegDO)
# devtools::load_all('M:/docs/SWMP/WtRegDO')

source('R/funcs.R')

######
# wqm data

# fls to import
fls <- list.files('ignore/', '^WQM.*xlsx$', full.names = TRUE)

# columns to select
cols <- c('STATION', 'Temp(C)', 'Pres(dbar)', 'Sal(PSU)', 'DO(mg/l)', 'Turbidity(NTU)', 'CHLa(ug/l)', 'CDOM(ppb-QSDE)', 'TIMESTAMP') 

# get all
dat <- vector('list', length(fls))
names(dat) <- fls
for(fl in fls){
  
  cat(fl, '\n')
  
  # import
  tmp <- read_excel(fl, sheet = 'forAccess')
  
  # cols to match
  tosel <- names(tmp)[names(tmp) %in% cols]
  nosel <- cols[!cols %in% tosel]
  toapp <- as.list(rep(NA, length(nosel)))
  names(toapp) <- nosel
  
  # get relevant columns
  tmp <- tmp[, tosel]
  
  # add empty columns if missing
  tmp <- data.frame(c(as.list(tmp), toapp))
  
  tmp$TIMESTAMP <- as.POSIXct(
    as.character(tmp$TIMESTAMP), format = '%Y-%m-%d %H:%M:%S', tz = 'America/Regina'
    )
  
  dat[[fl]] <- tmp
  
}

# reorder for combine
nms <- names(dat[[1]])
dat <- lapply(dat, function(x) x[, nms])
dat <- do.call('rbind', dat)
row.names(dat) <- 1:nrow(dat)
names(dat) <- c('stat', 'temp', 'pres', 'sal', 'do_mgl', 'turb', 'chla', 'cdom', 'datetimestamp')
dat$stat <- factor(dat$stat)
levels(dat$stat) <- c('P02', 'P05-B', 'P05-B', 'P05-S', 'P05-S', 'P05-S')
dat <- arrange(dat, stat, datetimestamp)

wqm_dat <- dat
save(wqm_dat, file = 'data/wqm_dat.RData')

# toplo <- tidyr::gather(dat, 'var', 'val', temp:cdom)
# ggplot(toplo, aes(x = datetimestamp, y = value)) +
#   geom_line() + 
#   facet_grid(variable ~ stat, scales = 'free_y') + 
#   theme_bw()

######
# PAR
# 
# fls <- list.files('ignore/', '^PAR', full.names = TRUE)
# 
# # get all
# dat <- vector('list', length(fls))
# names(dat) <- fls
# for(fl in fls){
#   
#   cat(fl, '\n')
#   
#   # import
#   tmp <- read_excel(fl, sheet = 'DATA')
#   
#   # timestamp in correct format
#   tmp$TimeStamp <- as.POSIXct(
#     as.character(tmp$TimeStamp), format = '%Y-%m-%d %H:%M:%S', tz = 'America/Regina'
#     )
#   
#   # create a unique identifier for replicates, reps are those within 10 s of each other
#   diffs <- which(c(diff(tmp$TimeStamp)) > 10) + 1
#   diffs <- c(1, diffs)
#   tmp$reps <- rep(1:(length(diffs)), times = diff(c(diffs, 1 + nrow(tmp))))
#   
#   # combine reps by average
#   tmp <- tmp[, c('Station', 'TimeStamp', 'PAR', 'reps')]
#   tmp <- aggregate(cbind(PAR, TimeStamp) ~ Station + reps, tmp, mean, 
#     na.rm = T)
#   
#   # time back to correct format
#   tmp$TimeStamp <- as.POSIXct(tmp$TimeStamp, tz = 'America/Regina', 
#     origin = '1970-01-01')
#   tmp <- tmp[, !names(tmp) %in% 'reps']
#   
#   dat[[fl]] <- tmp
#   
# }
# 
# dat <- do.call('rbind', dat)
# row.names(dat) <- 1:nrow(dat)
# 
# # standardize time step by station
# dat2 <- split(dat, dat$Station)
# dat2 <- lapply(dat2, setstep, date_col = 'TimeStamp', timestep = 30) 
# dat2 <- do.call('rbind', dat2)
# dat2 <- na.omit(dat2)
# row.names(dat2) <- 1:nrow(dat2)
# 
# ggplot(dat, aes(x = TimeStamp, y = PAR)) +
#   geom_point() + 
#   facet_grid(~ Station, scales = 'free_y') + 
#   theme_bw()
# 
# ggplot(dat2, aes(x = TimeStamp, y = PAR)) +
#   geom_point() + 
#   facet_grid(~ Station, scales = 'free_y') + 
#   theme_bw()
# 
# ######
# # ADCP
# fls <- list.files('ignore/', 'LOG', full.names = TRUE)
# 
# # get all
# dat <- vector('list', length(fls))
# names(dat) <- fls
# for(fl in fls){
#   
#   cat(fl, '\n')
#   
#   # import
#   tmp <- read_excel(fl, sheet = 'LOG8')
#   
#   dat[[fl]] <- data.frame(tmp)
#   
# }
# 
# dat <- do.call('rbind', dat)
# dat$deploy <- row.names(dat) %>% 
#   gsub('^ignore/|_000_000_LOG8.xlsx\\.[0-9].*$', '', .)
# row.names(dat) <- 1:nrow(dat)
# 
# # datetimestamp, select relevant cols
# datetimestamp <- with(dat, paste(paste(year, month, day, sep = '-'), paste(hour, minute, sec, sep = ':')))
# dat$datetimestamp <- as.POSIXct(datetimestamp, format = '%y-%m-%d %H:%M:%S', tz = 'America/Regina')
# dat <- dat[order(dat$datetimestamp), ]
# tosel <- c('datetimestamp', 'Dir', 'Mag', 'Depth', 'deploy')
# dat <- dat[, grepl(paste(tosel, collapse = '|'), names(dat))]
# 
# adcp_dat <- dat
# save(adcp_dat, file = 'data/adcp_dat.RData')

##
# decompose adcp data into primary axis from eigenvectors
# saved as adcp_datP

data(adcp_dat)

dat_in <- adcp_dat

# bins to decompose (all)
bins <- length(grep('^Dir', names(dat_in)))
bins <- c(1:bins)
  
# subset directions, mags by bins
dirs <- dat_in[, grep(paste(paste0('^Dir', bins), collapse = '|'), names(dat_in))] %>% 
  data.frame(datetimestamp = dat_in$datetimestamp, .) %>% 
  gather('variable', 'value', -datetimestamp) %>% 
  mutate(variable = gsub('^Dir', 'Bin', variable)) %>% 
  rename(
    bin = variable, 
    dir = value
    )
mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))] %>% 
  data.frame(datetimestamp = dat_in$datetimestamp, .) %>% 
  gather('variable', 'value', -datetimestamp) %>% 
  mutate(variable = gsub('^Mag', 'Bin', variable)) %>% 
  rename(
    bin = variable, 
    mag = value
    )
# join the two
dat <- full_join(dirs, mags, by = c('datetimestamp', 'bin'))

# get diff of observed data from rotation angle
diffN <- dat$dir - 360
diffE <- dat$dir - 90
  
# get magnitude of new vectors
dat$magsN <- dat$mag * cos(pi * diffN/180)
dat$magsE <- dat$mag * cos(pi * diffE/180)

##
#get  angle of primary

# get vector along primary
dat <- split(dat, dat$bin)
rots <- lapply(dat, function(x){
  
  # eigen decomp
  eig <- eigen(cov(na.omit(x[, c('magsN', 'magsE')])))
  
  # vector of primary
  vecs <- eig$vectors
  x$angs <- atan(vecs[2, 1]/vecs[1, 1]) * 180/pi
  
  # get magnitude along primary axis (use original vector)
  diffval <- x$dir - x$angs
  x$magsP <- x$mag * cos(pi * diffval/180)
  
  return(x)

}) 

dat <- do.call('rbind', rots)
rownames(dat) <- 1:nrow(dat)

# add pressure/height data from adcp_dat (repeated across bins)
hghts <- dat_in[, grep('datetimestamp|Dir|Depth', names(dat_in))] %>% 
  gather('variable', 'value', -datetimestamp, -Depth.mm.) %>% 
  mutate(variable = gsub('^Dir', 'Bin', variable)) %>% 
  rename(
    bin = variable, 
    dir = value
    ) %>% 
  select(datetimestamp, bin, Depth.mm.)

dat <- full_join(dat, hghts, by = c('datetimestamp', 'bin'))
adcp_datP <- dat
save(adcp_datP, file = 'data/adcp_datP.RData')

# ######
# # plot  
# plot_adcp(all_in = dat, shp_in = pbay, loc_in = c(-87.13208, 30.45682), 
#   fixed_y = FALSE)
# 
# ######
# # plot the eigenvectors 
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

######
# PAR

fls <- list.files('ignore/PAR/', recursive = TRUE, full.names = TRUE)

# get all
par_dat <- vector('list', length(fls))
names(par_dat) <- fls
for(fl in fls){
  
  cat(fl, '\n')
  
  # import, select columns, format timestamp
  tmp <- read_excel(fl, sheet = 'DATA') %>% 
    select(Station, TimeStamp, PAR) %>% 
    mutate(TimeStamp = as.POSIXct(as.character(TimeStamp), 
      format = '%Y-%m-%d %H:%M:%S', tz = 'America/Regina'
    )) %>% 
    rename(
      stat = Station, 
      datetimestamp = TimeStamp, 
      par = PAR
    )
  
  par_dat[[fl]] <- data.frame(tmp)
  
}

# combine list
par_dat <- do.call('rbind', par_dat)
row.names(par_dat) <- 1:nrow(par_dat)
par_dat$stat <- factor(par_dat$stat)
levels(par_dat$stat) <- c('P02', 'P05-B', 'P05-B', 'P05-S')
par_dat <- arrange(par_dat, stat, datetimestamp)

# combine with wqm data, must do by station
# set step at 30 minutes
data(wqm_dat)

stats <- unique(wqm_dat$stat)
out <- vector('list', length(stats))
names(out) <- stats
for(st in stats){
  
  wq_tmp <- filter(wqm_dat, stat %in% st)
  par_tmp <- filter(par_dat, stat%in% st)
  
  comb_tmp <- comb(wq_tmp, par_tmp, timestep = 30, date_col = 'datetimestamp', 
    method = 'union') %>% 
    select(-i.stat)
  comb_tmp$stat <- st
  
  out[[st]] <- comb_tmp
  
}

wqm_dat <- do.call('rbind', out) %>% 
  arrange(stat, datetimestamp) %>% 
  select(stat, datetimestamp, temp, pres, sal, do_mgl, turb, chla, cdom, par) %>% 
  mutate(stat = factor(stat, levels = c('P02', 'P05-S', 'P05-B')))

save(wqm_dat, file = 'data/wqm_dat.RData')

######
# get wx data from PNS
# combined with wqm_data, duplicated for each station

# # raw data from PNS using weatherData package
# library('weatherData')
# get_all <- getWeatherForDate(
#   'PNS',
#   '2014-04-04','2015-05-01',
#   opt_detailed = T,
#   opt_custom_columns = T,
#   custom_columns = c(1, 2, 5, 8, 13)
#   )
# 
# wx_dat <- get_all
# save(wx_dat, file = 'data/wx_dat.RData')
                  
# process wx data in correct format for metab
data(wx_dat)

wx_dat <- select(wx_dat, Time, TemperatureF, Sea_Level_PressureIn, Wind_SpeedMPH) %>% 
  rename(
    datetimestamp = Time, 
    ATemp = TemperatureF,
    BP = Sea_Level_PressureIn,
    WSpd = Wind_SpeedMPH
  ) %>% 
  mutate( # missing and text values to appropriate values
    datetimestamp = as.POSIXct(as.character(datetimestamp), format = '%Y-%m-%d %H:%M:%S', tz = 'America/Regina'),
    ATemp = as.numeric(replace(ATemp, ATemp == -9999, NA)),
    WSpd = replace(WSpd, grepl('^-9999', WSpd), NA),
    WSpd = as.numeric(replace(WSpd, WSpd %in% 'Calm', '0')),
    BP = as.numeric(replace(BP, BP == -9999, NA))
  ) %>% 
  mutate(
    ATemp = (ATemp - 32) * 5 / 9, # F to C
    WSpd = WSpd * 1609.34 / 3600, # mph to meters/s
    BP = BP * 33.86 # inches of hg to mb
  )

# combine with wqm_dat
# have to do station wise because of non-unique wx data
data(wqm_dat)

stats <- unique(wqm_dat$stat)
out <- vector('list', length(stats))
names(out) <- stats
for(st in stats){
  
  wq_tmp <- filter(wqm_dat, stat %in% st)
  
  comb_tmp <- comb(wq_tmp, wx_dat, timestep = 30, date_col = 'datetimestamp', 
    method = 'union')
  comb_tmp$stat <- st
  
  out[[st]] <- comb_tmp
  
}

wqm_dat <- do.call('rbind', out) %>% 
  arrange(stat, datetimestamp) %>% 
  mutate(stat = factor(stat, levels = c('P02', 'P05-S', 'P05-B')))

save(wqm_dat, file = 'data/wqm_dat.RData')

######
# metabolism data
rm(list = ls())

data(wqm_dat)

# put data in format that works with ecometab function from WtRegDO
toproc <- dplyr::rename(wqm_dat, 
    DateTimeStamp = datetimestamp,
    Temp = temp, 
    Depth = pres, 
    Sal = sal, 
    DO_mgl = do_mgl
  ) %>% 
  select(-turb, -chla, -cdom, -par)
  
# metadata for each station, required for ecometab
metas <- list(
  `P02` = c(30.54035, -87.16067, 2.064245), 
  `P05-S` = c(30.45682, -87.13208, 3.502017/2),
  `P05-B` = c(30.45682, -87.13208, 3.502017/2)
)

stats <- unique(toproc$stat)
out <- vector('list', length(stats))
names(out) <- stats
for(st in stats){
  
  cat(st, '\t')
  
  wq_tmp <- filter(toproc, stat %in% st) %>% 
    mutate(stat = NULL)
  
  # do not use air/sea exhange of bottom
  bott <- F
  if(st %in% 'P05-B') bott <- T
  
  met_tmp <- WtRegDO::ecometab(wq_tmp, lat = metas[[st]][1], long = metas[[st]][2], 
    tz = 'America/Regina', depth_val = metas[[st]][3], bott_stat = bott)
  
  out[[st]] <- met_tmp
  
}

met_dat <- out
save(met_dat, file = 'data/met_dat.RData')

######
# additional wqm post-processing to remove bogus data

data(wqm_dat)

# 18-28 May P02 bad salinity
dts <- as.POSIXct(c('2014-05-18 0:0', '2014-05-29 0:0'), format = '%Y-%m-%d %H:%M', tz = 'America/Regina')
vec <- with(wqm_dat, datetimestamp <= dts[2] & datetimestamp >= dts[1] & stat %in% 'P02')
wqm_dat$sal <- replace(wqm_dat$sal, vec, NA)

# 13-17 Aug may P05-B bad salinity
dts <- as.POSIXct(c('2014-08-13 0:0', '2014-08-18 0:0'), format = '%Y-%m-%d %H:%M', tz = 'America/Regina')
vec <- with(wqm_dat, datetimestamp <= dts[2] & datetimestamp >= dts[1] & stat %in% 'P05-B')
wqm_dat$sal <- replace(wqm_dat$sal, vec, NA)

# remove optics 7/16 to 8/6 for P05-S
dts <- as.POSIXct(c('2014-07-16 0:0', '2014-08-07 0:0'), format = '%Y-%m-%d %H:%M', tz = 'America/Regina')
vec <- with(wqm_dat, datetimestamp <= dts[2] & datetimestamp >= dts[1] & stat %in% 'P05-S')
wqm_dat$chla <- replace(wqm_dat$chla, vec, NA)
wqm_dat$cdom <- replace(wqm_dat$cdom, vec, NA)
wqm_dat$turb <- replace(wqm_dat$turb, vec, NA)

# spikes in CDOM all sites
tmp <- split(wqm_dat, wqm_dat$stat)
tmp <- lapply(tmp, function(x) {
  
  qts <- quantile(x$cdom, c(0.01, 0.99), na.rm = TRUE)
  x$cdom <- with(x, replace(cdom, cdom < qts[1] | cdom > qts[2], NA))
  return(x)
  
  })
tmp <- do.call('rbind', tmp) %>% 
  arrange(stat, datetimestamp)
wqm_dat <- tmp

save(wqm_dat, file = 'data/wqm_dat.RData')

######
# CTD

# # copy files from L drive
# fls <- list.files('L:/lab/SPAM2/CTD/Processing', 'CTD.*\\.xls', recursive = T, full.names = TRUE)
# fls <- grep('dy\\.xls$', fls, value = T, invert = T)
# file.copy(fls, 'M:/docs/SPAM_2/ignore/')
# 
# # get all
# fls <- list.files('ignore/', '^CTD', full.names = TRUE)
# 
# dat <- vector('list', length(fls))
# names(dat) <- fls
# for(fl in fls){
#   
#   cat(fl, '\n')
#   
#   # import
#   tmp <- read_excel(fl, sheet = 'forAccess')
#   tmp <- tmp[, !grepl('LAYER|PAR|^N$|^RSQ$|pH', names(tmp))]
#   
#   dat[[fl]] <- data.frame(tmp)
#   
# }
# 
# dat <- do.call('rbind', dat)
# row.names(dat) <- 1:nrow(dat)
# ctd_dat <- form_dat(dat)
# ctd_dat <- arrange(ctd_dat, Date, Station, Depth)
# 
# save(ctd_dat, file = 'data/ctd_dat.RData')

##
# additional ctd post-processing to remove bogus data
data(ctd_dat)

# bottom bumps of CTD in April 2014 for fluor and turb
dt <- as.Date('2014-04-21')
stat <- 'P04'
depth <- 2.75
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Fluor'] <- 1.88  
ctd_dat[sel, 'Turb'] <- 5.09 

stat <- 'P03'
depth <- 2.25
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Turb'] <- 4.0784377

# bottom bumps of CTD in Sep 2014 for fluor and turb
dt <- as.Date('2014-09-17')
stat <- 'P05'
depth <- 4
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Fluor'] <- 2.1746 # val at next highest reading
ctd_dat[sel, 'Turb'] <- 3.3189341 # val at next highest reading

stat <- 'P06'
depth <- 5.5
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Fluor'] <- 1.3547 # val at next highest reading
ctd_dat[sel, 'Turb'] <- 4.9427779 # val at next highest reading

stat <- 'P07'
depth <- 9.25
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Fluor'] <- 0.2919 # val at next highest reading
ctd_dat[sel, 'Turb'] <- 1.5358241 # val at next highest reading

# bottom bumps of CTD in Oct 2014 for fluor and turb
dt <- as.Date('2014-10-15')
stat <- 'P05'
depth <- 4.25
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Fluor'] <- 2.2299 # val at next highest reading
ctd_dat[sel, 'Turb'] <- 2.680488 # val at next highest reading

# bottom bumps of CTD in Dec 2014 for fluor and turb, p5 and p6
dt <- as.Date('2014-12-10')
stat <- 'P05'
depth <- 4
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Fluor'] <- 2.7677
ctd_dat[sel, 'Turb'] <- 7.4263357
depth <- 3.75
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Turb'] <- 7.4263357

stat <- 'P06'
depth <- 5.5
sel <- with(ctd_dat, Date == dt & Station %in% stat & Depth %in% depth)
ctd_dat[sel, 'Fluor'] <- 1.2711
ctd_dat[sel, 'Turb'] <- 2.1999093

save(ctd_dat, file = 'data/ctd_dat.RData')
