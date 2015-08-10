######
# SPAM 2 processing of raw data

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
# devtools::load_all('M:/docs/SWMPr')
library(SWMPr)

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
levels(dat$stat) <- c('P02', 'P05-B', 'P05-B', 'P05-S', 'P05-S')
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
# 
# ######
# # CTD
# 
# # get file, I had to manually edit some entries 
# ctd_dat <- read_excel('ignore/MasterData.xls', sheet = 'MASTER', col_names = F)
# ctd_dat <- form_dat(as.data.frame(ctd_dat))
# 
# save(ctd_dat, file = 'data/ctd_dat.RData')
# 
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
