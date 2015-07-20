######
# ADCP

source('R/funcs.R')
library(maptools)
library(ggplot2)


load('data/adcp_dat.RData')
shp <- readShapeSpatial('M:/GIS/PBay_diss.shp')
loc <- c(-87.13208, 30.45682)
coord_lims <- list(c(-87.2, -87), c(30.4, 30.5))

plot_adcp(adcp_dat[3, ], adcp_dat, shp_in =  shp, loc_in = loc)

plot_press(adcp_dat[1, ], adcp_dat)

pdf('figs/test.pdf', width = 6.5, height = 12, family = 'serif')
# tiff('figs/test.tif', width = 8, height = 7, units = 'in', res = 300, compression = 'lzw', family = 'serif')

get_multi(adcp_dat[adcp_dat$deploy == 'P050914', ], shp_in = shp, loc_in = loc, interp = 4, col_vec = c('red', 'green'))

dev.off()

