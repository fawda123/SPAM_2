######
# SPAM 2 processing of raw data

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

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

# toplo <- tidyr::gather(dat, 'var', 'val', temp:cdom)
# ggplot(toplo, aes(x = datetimestamp, y = val)) +
#   geom_line() + 
#   facet_grid(var ~ stat, scales = 'free_y') + 
#   theme_bw()

######
# PAR



