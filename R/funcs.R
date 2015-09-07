######
# rotate adcp vector to a direction
#
# dat_in input adcp data
# theta direction to rotate (0/360 is north)
#
# requires tidyr
vecrots <- function(dat_in, theta = 360){
  
  # bins to rotate (all)
  bins <- length(grep('^Dir', names(dat_in)))
  bins <- c(1:bins)

  # subset directions, mags by bins
  dirs <- dat_in[, grep(paste(paste0('^Dir', bins), collapse = '|'), names(dat_in))] %>% 
    data.frame(datetimestamp = dat_in$datetimestamp, .) %>% 
    gather('variable', 'value', -datetimestamp)
  mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))] %>% 
    data.frame(datetimestamp = dat_in$datetimestamp, .) %>% 
    gather('variable', 'value', -datetimestamp)

  # get diff of observed data from rotation angle
  diffval <- dirs$value - theta
  
  # get magnitude of new vectors
  magsrot <- mags$value * cos(pi * diffval/180)
  
  # organize output
  mags$value <- magsrot
  mags <- spread(mags, variable, value)
  dirs$value <- theta
  dirs <- spread(dirs, 'variable', 'value')
  
  # make like dat_in
  out <- full_join(dirs, mags, by = 'datetimestamp') %>% 
    mutate(Depth.mm. = dat_in$Depth.mm., deploy = dat_in$deploy)
  out <- out[, names(dat_in)]
  
  return(out)
  
}

######
# get distance of parcels from adcp and cumulative distance travelled by bin
# uses adcp_datP as input and principal vector estimated in dat_proc
# assumes a fixed two hour timestep (7200 seconds)
# cumulative distance is relative only to the estimated distance at each step
#
# dat_in adcp_datP input
# timestep timestep interval in seconds for multiplying
#
# requires dplyr
vecdist <- function(dat_in, timestep = 7200, sepout = FALSE){
  
  # cumulative sum will not work with NA
  dat <- na.omit(dat_in)
  
  # format the data
  dat <- group_by(dat_in, bin) %>% 
    arrange(datetimestamp) %>% 
    mutate(
      DisP = SWMPr::smoother(magsP, 2)[, 1] * timestep, # distance is integration of speed between steps
      cDisP = cumsum(DisP)
      ) %>% 
    ungroup %>% 
    as.data.frame

  return(dat)
  
}

######
# plot adcp data, shows observed vector and rotated based on principal axis, uses adcp_datP
#
# Depths in plot are distance from surface based on input data
#
# dat_in input row to plot
# all_in all adcp data
# shp_in SpatialPolygonsDataFrame input for map
# loc_in location of ADCP in reference to shp_in
# bins depth bins to plot
# z_bins depth (m) of each bin (bin 1 is the bottom), defaults to 0.5 meter spacing with the first at 1m from the transducer (from report, transducer is assumed to be the bottom), reported as depth from surface on the plot in reference to the total depth
# z_tot total depth of location (m)
# coord_lims list of constraining x, y locations for plotting shp_in
# arrow end of arrow size
# barmin lower axis limit on barplot
# barmax upper axis limit on barplot
# barcol vector of colors for bars on barplot
# vec_scl scaling value for vector, for better viz
#
# requires ggplot2
#
plot_adcp <- function(dat_in = NULL, all_in, shp_in, loc_in, bins = 1:5, 
  z_bins = NULL, z_tot = 3.5, coord_lims = NULL, arrow = 0.2, barmin = 0, 
  barmax = NULL, barcol = NULL, vec_scl = 5, ...){
    
  # defaults to time of first obs
  if(is.null(dat_in)){
    sel <- all_in$datetimestamp %in% unique(all_in$datetimestamp)[1]
    dat_in <- all_in[sel, ]
  }
  
  # subset by bins
  dat_in <- dat_in[dat_in$bin %in% paste0('Bin', bins), ]
  
  # get depth of bins in relation to surface
  if(is.null(z_bins)){
    z_bins <- cumsum(c(1, rep(0.5, length = length(bins) - 1)))
    z_bins <- z_tot - z_bins
  }
  
  # dir , mag
  dirs <- dat_in$dir
  mags <- dat_in$mag
  
  # get barrng from data if not supplied
  if(is.null(barmax)) barmax <- max(mags)
  
  # colors of directions, based on angle
  dircol <- rainbow(360)[1 + dirs] # min direction is zero
  
  # color of bars, based on relative magnitude of mags
  if(is.null(barcol)){
    barcol <- c('tomato1', 'lightgreen')
    barcol <- col_fun(mags, barcol)
  }
  
  # change axis reference for directions
  dirs[dirs > 90] <- 360 - dirs[dirs > 90] + 90
  dirs[dirs <= 90] <- 90 - dirs[dirs <= 90]
  
  # x, y locs from polar coords
  xvals <- mags * cos(pi * dirs / 180)
  yvals <- mags * sin(pi * dirs / 180) 
  
  ## vector rotation based on theta
  
  # theta input is zero north, running clockwise
  # have to change reference for trig funcs
  theta <- dat_in$angs
  theta[theta > 90] <- 360 - theta[theta > 90] + 90
  theta[theta <= 90] <- 90 - theta[theta <= 90]
  
  # find values in polar coords given angle and mag
  xrot <- with(dat_in, magsP * cos(pi * theta/180))
  yrot <- with(dat_in, magsP * sin(pi * theta/180))

  # setup plot data
  bin_labs <- paste('Bin', paste(bins, z_bins, sep = ': '), 'm') 
  vecs <- data.frame(
    long1 = loc_in[1], 
    lat1 = loc_in[2],
    long2 = xvals + loc_in[1], 
    lat2 = yvals + loc_in[2],
    longrot = xrot + loc_in[1], 
    latrot = yrot + loc_in[2],
    bins = bin_labs,
    z_bins = z_bins,
    mags = mags,
    magsP = dat_in$magsP
    )
  vecs$bins <- factor(vecs$bins, levels = rev(bin_labs))
  loc_in <- data.frame(long = loc_in[1], lat = loc_in[2])
  
  # dir plot
  p1 <- suppressMessages({ggplot(loc_in, aes(x = long, y = lat)) + 
    coord_fixed(xlim = sp::bbox(shp_in)[1, ], ylim = sp::bbox(shp_in)[2, ]) + 
    geom_polygon(data = shp_in, aes(x = long, y = lat, group = group), 
      fill = 'lightgrey') +
    geom_segment(data = vecs, aes(x = long1, y = lat1, xend = long2, yend = lat2, 
      colour = bins), size = 1.5, alpha = 0.6) +
    geom_segment(data = vecs, aes(x = long1, y = lat1, xend = longrot, yend = latrot)) + 
    geom_point() +
    scale_colour_manual(values = dircol) + 
    theme_classic() + 
    theme(legend.position = 'none') +
    xlab('Long') +
    ylab('Lat') + 
    facet_wrap(~bins, ncol = 1)
  })
  
  # constrain plot boundaries
  if(!is.null(coord_lims))
    p1 <- p1 + coord_map(
      xlim = coord_lims[[1]],
      ylim = coord_lims[[2]]
    )

  # mag plot
  p2 <- ggplot(vecs, aes(x = z_bins, y = mags)) + 
    geom_bar(stat = 'identity', aes(fill = bins, colour = bins)) + 
    facet_wrap(~bins, ncol = 1) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(), 
      axis.text.y = element_text(size = 6), 
      legend.position = 'none'
      ) + 
    scale_y_continuous(limits = c(barmin, barmax)) + 
    scale_x_continuous(trans = "reverse", breaks = unique(vecs$z_bins)) + 
    scale_fill_manual(values = as.character(barcol)) + 
    scale_colour_manual(values = as.character(barcol)) +
    coord_flip() +
    ylab('Velocity')

  # pressure plot
  p3 <- plot_press(dat_in, all_in, ...) 
  p3 <- p3 + theme(plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0.4), "in"))
  
  # combined grob
  gridExtra::grid.arrange(
    p3, 
    gridExtra::arrangeGrob(p1, p2, ncol = 2, widths = c(1, 1)), 
    ncol = 1,
    top = as.character(dat_in$datetimestamp), 
    heights = c(0.2, 1)
  )
 
}

######
# plot the pressure time series within a window
#
# dat_in row of adcp data at center of plot
# all_in all adcp data
# win window around center of plot, default two days
# fixed_y as logical will fix the y axis using the whole range of depth variable or only the range within the x window
plot_press <- function(dat_in, all_in, win = NULL, fixed_y = TRUE){

  # window defaults to one day
  if(is.null(win))
    win <- 60 * 60 * 24
  
  # subset by one bin (depth is repeated for each bin)
  binsel <- unique(all_in$bin)[1]
  dat_in <- dat_in[dat_in$bin %in% binsel, ]
  all_in <- all_in[all_in$bin %in% binsel, ]
  
  # x limits of plot from window
  lims <- with(dat_in, c(datetimestamp - win, datetimestamp + win))
  
  if(fixed_y){
    ylims <- range(all_in$Depth.mm./1000, na.rm = TRUE)
  } else {
    ylims <- all_in[all_in$datetimestamp >= lims[1] & all_in$datetimestamp <= lims[2], ]
    ylims <- range(ylims$Depth.mm./1000, na.rm = TRUE)
  }
  
  # the plot and return
  p1 <- ggplot(dat_in, aes(x = datetimestamp, y = Depth.mm./1000)) +
    geom_line(data = all_in, aes(x = datetimestamp, y = Depth.mm./1000)) +
    geom_point(colour = 'lightgreen', size = 4) +
    scale_x_datetime('Time stamp', limits = lims) +
    scale_y_continuous('Depth (m)', limits = ylims) +
    theme_classic()
  
  return(p1)
  
}

######
# plot adcp data, same as plot_adcp but only shows raw vectors, I copied this because I'm lazy
#
# Depths in plot are distance from surface based on input data
#
# dat_in input row to plot
# all_in all adcp data
# shp_in SpatialPolygonsDataFrame input for map
# loc_in location of ADCP in reference to shp_in
# bins depth bins to plot
# z_bins depth (m) of each bin (bin 1 is the bottom), defaults to 0.5 meter spacing with the first at 1m from the transducer (from report, transducer is assumed to be the bottom), reported as depth from surface on the plot in reference to the total depth
# z_tot total depth of location (m)
# coord_lims list of constraining x, y locations for plotting shp_in
# arrow end of arrow size
# vec_scl scaling value for vector, for better viz
#
# requires ggplot2
#
plot_adcpraw <- function(dat_in, shp_in, loc_in, bins = 1:5, bin_labs = NULL, 
  z_bins = NULL, z_tot = 3.5, coord_lims = NULL, arrow = 0.2, vec_scl = 1, ...){

  # subset by bins
  dat_in <- dat_in[, grepl(paste(c('datetimestamp', bins), collapse = '|'), names(dat_in))]
  
  # get depth of bins in relation to surface
  if(is.null(z_bins)){
    z_bins <- cumsum(c(1, rep(0.5, length = length(bins) - 1)))
    z_bins <- z_tot - z_bins
  }
  
  # dir , mag
  dirs <- as.numeric(dat_in[, grepl('^Dir', names(dat_in))])
  mags <- as.numeric(dat_in[, grepl('^Mag', names(dat_in))])
  # colors of directions, based on angle
  dircol <- rainbow(360)[round(1 + dirs)] # min direction is zero
  
  # change axis reference for directions
  dirs[dirs > 90] <- 360 - dirs[dirs > 90] + 90
  dirs[dirs <= 90] <- 90 - dirs[dirs <= 90]
  
  # x, y locs from polar coords
  xvals <- vec_scl * mags * cos(pi * dirs / 180)
  yvals <- vec_scl * mags * sin(pi * dirs / 180) 

  # setup plot data
  if(is.null(bin_labs))
    bin_labs <- paste('Bin', paste(bins, z_bins, sep = ': '), 'm') 
  vecs <- data.frame(
    long1 = loc_in[1], 
    lat1 = loc_in[2],
    long2 = xvals + loc_in[1], 
    lat2 = yvals + loc_in[2],
    bins = bin_labs,
    z_bins = z_bins
    )
  vecs$bins <- factor(vecs$bins, levels = rev(bin_labs))
  loc_in <- data.frame(long = loc_in[1], lat = loc_in[2])
  
  # dir plot
  p1 <- suppressMessages({ggplot(loc_in, aes(x = long, y = lat)) + 
    coord_fixed(xlim = sp::bbox(shp_in)[1, ], ylim = sp::bbox(shp_in)[2, ]) + 
    geom_polygon(data = shp_in, aes(x = long, y = lat, group = group), 
      fill = 'lightgrey') +
    geom_segment(data = vecs, aes(x = long1, y = lat1, xend = long2, yend = lat2, colour = bins), 
      size = 1.5, alpha = 0.6) +
    geom_point() +
    scale_colour_manual(values = dircol) + 
    theme_classic() + 
    theme(legend.position = 'none') +
    xlab('Long') +
    ylab('Lat') + 
    facet_wrap(~ bins, ncol = 1)
  })
  
  # constrain plot boundaries
  if(!is.null(coord_lims))
    p1 <- p1 + coord_map(
      xlim = coord_lims[[1]],
      ylim = coord_lims[[2]]
    )

  return(p1)
 
}

######
# plot adcp direction and magnitude data for selected bins over a time period

# all_in all adcp data
# bins depth bins to plot
# rng time window of dates plot as chr string with two elements each of the form yyyy-mm-dd
# z_bins depth (m) of each bin (bin 1 is the bottom), defaults to 0.5 meter spacing with the first at 1m from the transducer (from report, transducer is assumed to be the bottom), reported as depth from surface on the plot in reference to the total depth
# z_tot total depth of location (m)
# tz timezone 
#
# requires ggplot2, tidyr 
#
plot_adcp2 <- function(all_in, bins = 1:5, rng = NULL, z_bins = NULL, z_tot = 3.5, tz = 'America/Regina', 
  cols = NULL, ...){
  
  # get range of times for plot
  if(is.null(rng)){
    
    rng <- with(all_in, c(datetimestamp[1], datetimestamp[100]))
    
  } else {
    
    if(length(rng) != 2) stop('rng must have two values')
    rng <- as.POSIXct(rng, format = '%Y-%m-%d', tz = tz)
     
  }
  
  # get depth of bins in relation to surface
  if(is.null(z_bins)){
    z_bins <- cumsum(c(1, rep(0.5, length = length(bins) - 1)))
    z_bins <- z_tot - z_bins
  }
  
  # subset data 
  nms_mag <- c('datetimestamp', paste0('Mag', bins))
  nms_dir <- gsub('^Mag', 'Dir', nms_mag)
  vec <- with(all_in, datetimestamp >= rng[1] & datetimestamp <= rng[2])
  toplo_mag <- all_in[vec, nms_mag]
  toplo_dir <- all_in[vec, nms_dir]
  
  # long-form for plotting
  toplo_mag <- gather(toplo_mag, 'bin', 'mags', -datetimestamp) %>% 
    mutate(bin =  gsub('^Mag', 'Bin', bin))
  toplo_dir <- gather(toplo_dir, 'bin', 'dirs', -datetimestamp) %>% 
    mutate(bin = gsub('^Dir', 'Bin', bin))
  toplo <- full_join(toplo_mag, toplo_dir, by = c('datetimestamp', 'bin'))# %>% 
    # gather('var', 'val', mag:dir)
  
  # change axis reference for directions
  toplo$dirs[toplo$dirs > 90] <- with(toplo, 360 - dirs[dirs > 90] + 90)
  toplo$dirs[toplo$dirs <= 90] <- with(toplo, 90 - dirs[dirs <= 90])
  
  # x, y locs from polar coords
  xvals <- with(toplo, mags * cos(pi * dirs / 180))
  yvals <- with(toplo, mags * sin(pi * dirs / 180))
  
  # get segment start, end for time series plot of vecs
  sclval <- diff(range(as.numeric(toplo$datetimestamp)))
  toplo <- data.frame(toplo, xvals, yvals) %>% 
    mutate(
      xvals1 = datetimestamp,
      xvals2 = datetimestamp + sclval * xvals, 
      yvals1 = 0, 
      yvals2 = yvals
    ) %>% 
    select(-xvals, -yvals)
  
  # default cols
  if(is.null(cols)) cols <- rev(RColorBrewer::brewer.pal(9, 'Set1')[1:3])
  
  # the plot
  p <- ggplot(toplo) +
    geom_segment(aes(x = xvals1,  y = yvals1, xend = xvals2, yend = yvals2, colour = mags), size = 1) + 
    facet_wrap( ~ bin, ncol = 1) + 
    scale_colour_gradientn(colours = cols) + 
    theme_bw() + 
    theme(
      legend.position = 'none',
      axis.title.y = element_blank(), 
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank()
      ) + 
    scale_x_datetime('Date')
    
  return(p)
  
}

######
# 
# dat_in input row to plot
# shp_in SpatialPolygonsDataFrame input for optional map
# loc_in location of ADCP in reference to shp_in
# col_vec colors to use for color ramp on bar plot
# ... other arguments passed to plot_adcp
get_multi <- function(dat_in, col_vec, shp_in, loc_in, interp = NULL, bins = 1:6, ...){
  
  # remove depth column
  dat_in <- dat_in[, grep('^Mag|^Dir|datetimestamp|^Depth', names(dat_in))]
  
  # interp values for smoother plots
  if(!is.null(interp)){

    # interp
    interp <- nrow(dat_in) * interp
    tzone <- attr(dat_in$datetimestamp, 'tz')
    dat_in$datetimestamp <- as.numeric(dat_in$datetimestamp)
    dat_in <- apply(dat_in, 2, function(x){
      x <- as.numeric(x)
      approx(x, n = interp)$y
    })
    
    dat_in <- data.frame(dat_in)
    dat_in$datetimestamp <- as.POSIXct(dat_in$datetimestamp, tzone = tzone, origin = '1970-01-01')
    
  }

  # console percent
  cat('\n% complete...\n\n')
  counts <- round(seq(1, nrow(dat_in), length = 20))

  # get bins if not provided
  if(is.null(bins)) 
    bins <- 1:length(grep('^Dir', names(dat_in)))
  
  # get barmax based on all data
  mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))]
  barmax <- max(mags)

  # get color vectors
  nc <- ncol(mags)
  nr <- nrow(mags)
  cols <- unlist(mags)
  cols <- col_fun(cols, col_vec)
  cols <- matrix(cols, nrow = nr, ncol = nc)

  # pass each row to plot_adcp
  for(row in 1:nrow(dat_in)){
    
    # console output
    perc <- 5 * which(row == counts)
    if(length(perc) != 0) cat(perc, '\t')
    
    # barplot cols
    barcol <- cols[row, ]
    
    # create plot
    plot_adcp(dat_in[row, ], dat_in, shp_in, loc_in, bins = bins, barmax = barmax, barcol = barcol, ...)
    
  }
  
}

######
# vals_in values to link to color
# cols_in colors for color ramp
col_fun <- function(vals_in, cols_in){

  vals <- scales::rescale(vals_in, c(0, 1))
  cols <- colorRamp(cols_in)(vals)
  apply(cols, 1, function(x) rgb(x[1], x[2], x[3], max = 255))
  
}

######
# functions for SPAM_2 CTD reports
# created May 2014, M. Beck

######
# format master data file
form_dat <- function(dat.in){
  
  out <- dat.in
  
  # cumulative distance for each station
  sta_lens <- data.frame(
    Station = paste0('P0', seq(1,9)),
    dist = 1.60934 * cumsum( # as km
      c(0, 2.588, 2.022, 1.711, 2.571, 3.076, 4.233, 3.656, 3.913))
  )
  
  names(out) <- c('Station', 'Date', 'Depth', 
    'Temp', 'Salinity', 'SigmaT', 'DO', 'DOsat', 'Fluor', 'Turb', 'CDOM')
  
  # format datetimestamp
  out$DateTimeStamp <- as.POSIXct(
    as.character(out$Date), 
    format = '%Y-%m-%d %H:%M:%S', 
    tz = 'America/Regina'
  )
  
  # reformat date column to date
  out$Date <- as.Date(out$DateTimeStamp)
  
  # merge out with dist (nm) between stations
  out <- merge(out, sta_lens, by = 'Station', all.x = T)
  
  # floor at zero if values are negative
  # only do for salinity, DO, DOsat, Fluor, Turb, CDOM
  out$Salinity <- pmax(0, out$Salinity)
  out$DO <- pmax(0, out$DO)
  out$DOsat <-  pmax(0, out$DOsat)
  out$Fluor <-  pmax(0, out$Fluor)
  out$Turb <-  pmax(0, out$Turb)
  out$CDOM <- pmax(0, out$CDOM)
    
  return(out)
  
}  

######
# get min, max values for each cruise date and parameter
# 'dat_in' is list of data frame as output from form_dat
get_rngs <- function(dat_in){
  
  dat_in <- do.call('rbind', dat_in)
 
  out <- apply(dat_in[,c('Temp', 'Salinity', 'SigmaT', 'DO', 'DOsat', 
      'Fluor', 'Turb', 'CDOM')], 
    2, range)
  
  out <- data.frame(out)
  
  return(out)
  
  }

######
# create contour plots for CTD data, linear interpolation
# input 'dat_in' is one date of ctd data along the tidal axis
# 'var_plo' is variable to plot, e.g., 'Temp'
# 'rngs_in' is output from 'get_rngs' above
# 'num_levs' is number of contours
# 'xlab' and 'ylab' are for axis labels
# 'var_lab' logical for variable text on plot
# 'cols' colors to use, passed to colorRampPalette
# 'ncol' number for smoothing colors in plot
ctd_plot <- function(dat_in, var_plo, rngs_in = NULL, num_levs = 8, ylab = 'Depth (m)',
  xlab = 'Channel distance from P01 to P09 (km)', var_lab = TRUE, 
  cols = c('tomato', 'lightblue', 'lightgreen','green'),
  ncol = 100){

  library(scales)
  library(zoo)
  library(reshape2)
  library(fields)
  
  # for matrix rotation, otherwise plot is not correct
  rotate <- function(x) t(apply(x, 2, rev))
  
  # maximum depth of data for each station, used for polygon masking
  # value is linearly interpolated to increase samp
  data(depth_tran)
  maxd <- depth_tran
  
  # size of intepolation grid on one axis
  num_int <-200 #nrow(maxd)
  
  # get relevant data from input, convert units for some
  dat_in <- dat_in[, c('Station', 'Depth', var_plo, 'dist')]
  
  dat_in$Depth <- -1 * dat_in$Depth
  dat_in$dist <- dat_in$dist * 1.6093
  
  # for plotting station location 
  top <- unique(dat_in[, c('Station', 'dist')])
  
  # convert table long to short
  dat_in <- dcast(dat_in, Depth ~ dist, value.var = var_plo)
  
  # complete depth to surface
  if(!0 %in% dat_in$Depth) 
    dat_in <- rbind(dat_in, c(0, rep(NA, ncol(dat_in)-1)))
  
  # add rows for max depth of polygon box
  add_dep <- seq(min(maxd$mllw_m), min(dat_in$Depth) - 0.25, by = 0.25)
  add_dep <- c(add_dep, rep(NA, length = length(add_dep) * (ncol(dat_in)-1)))
  add_dep <-  matrix(add_dep, ncol = ncol(dat_in), byrow = F)
  add_dep <- as.data.frame(add_dep); names(add_dep) <- names(dat_in)
  dat_in <- rbind(add_dep, dat_in)
  
  # fill leading NA with earliest obs value
  dat_in <- na.locf(dat_in)
  
  # flip by depth, fill trailing NA with last obs value
  dat_in <- dat_in[order(dat_in$Depth, decreasing = T), ]
  dat_in <- na.locf(dat_in)
  
  # x, y values for linear interp grid, long form
  new_grd <- expand.grid(
      approx(dat_in$Depth, n = num_int)$y, 
      approx(as.numeric(names(dat_in)[-1]), n = num_int)$y
      )
  # get interped values
  int_val <- interp.surface(
    obj = list(  
      x = dat_in$Depth, 
      y = as.numeric(names(dat_in)[-1]), 
      z = dat_in[,-1]), 
    loc = new_grd
    )
  
  # combine coords with interp values
  new_grd <- data.frame(new_grd, int_val)
  new_grd <- dcast(new_grd, Var1 ~ Var2, 
    value.var = 'int_val')
  
  # coords for plot
  y.val <- new_grd$Var1
  x.val <- as.numeric(names(new_grd)[-1])
  z.val <- as.matrix(new_grd[order(new_grd$Var1, decreasing = T),-1])
  
  ##
  # start plot
  plot.new()
  
  # number of contours
  levs <- num_levs
  
  # mask z.val so correct col contours show up
  mask_grd <- sapply(1:num_int,
    function(x){
      dep_chk <- approx(maxd$mllw_m, n = num_int)$y
      out <- rep(NA, num_int)
      out[rev(y.val) >= (dep_chk[x])- 0.75] <- 1
      return(out)
    }
  )
  z.val <- ifelse(mask_grd, z.val, NA)
  
  # add values for continuous colour ramp
  if(!is.null(rngs_in)){
    if(var_plo %in% names(rngs_in)){
      z.val[200, 1] <- rngs_in[, var_plo][1]
      z.val[200, 2] <- rngs_in[, var_plo][2]
      }
    }
  
  # plot margins
  par(new = "TRUE",plt = c(0.15,0.85,0.3,0.9),las = 1,cex.axis = 1)
  
  # color function
  in_col <- colorRampPalette(cols)
  
  # contour plot with isolines
  filled.contour3(x = x.val, y = y.val, z = rotate(z.val),
    color.palette = in_col,
    ylab = ylab, xlab='',
    nlevels = ncol, # for smoothed colors
    axes = F)
  contour(x = x.val, y = y.val, z = rotate(z.val), nlevels=levs,
    axes = F, add = T)
  
  ##
  # axis labels
  
  # xlab
  mtext(text = xlab, side = 1, line = 3)
  
  ##
  # axes
  
  # x
  axis(side = 1)
  
  # y
  y.axs <- axTicks(2, par('yaxp'))
  axis(side = 2, at = y.axs, labels = abs(y.axs))
  
  # top
  axis(side = 3, at = top$dist, labels = top$Station, cex.axis = 0.7,
    tick = F, line = -1)
  
  # masking depth
  poly.x <- approx(x.val, n = nrow(maxd))$y
  with(maxd,
    polygon(
      c(poly.x, rev(poly.x)), 
      c(mllw_m, rep(min(mllw_m), length(mllw_m))), 
      col = alpha('grey', 1), 
      border = NA
      ))
  
  ##
  # variable name -lower left
  if(var_lab)
    text(x = par('usr')[1], par('usr')[3] + 1, labels = var_plo, pos = 4, 
      cex = 1.5)
  
  box()
  
  ##
  # legend
  par(new = "TRUE", plt = c(0.87,0.91,0.3,0.9),las = 1,cex.axis = 1)
  filled.legend(x.val,y.val,rotate(z.val),color=in_col,xlab = "",
    nlevels = levs,
    ylab = "",xlim = c(min(xintercepts),max(xintercepts)),
    ylim = c(min(z.val),max(z.val)))
  
  }

######
# plot bottom DO from CTD by distance and time  
# returns an interpolated two contour plot using methods similar to ctd_plot
#
# dat_in input ctd data
# num_levs number of contour levels
# ylab labels for y axis
# col color vector
# ncol number of colors for smoothing plot
ctd_bott <- function(dat_in, num_levs = 8, ylab = 'Axial distance (km)',
  cols = c('tomato', 'lightblue', 'lightgreen','green'),
  ncol = 100){
  
  library(dplyr) 
  
  do_mat <- select(dat_in, Station, Date, Depth, DO, dist) %>% 
    group_by(Station, Date) %>% 
    mutate(maxd = max(Depth, na.rm = T)) %>% 
    filter(Depth == maxd) %>%
    ungroup %>% 
    select(Date, dist, DO) %>% 
    spread(Date, DO) %>% 
    data.frame
  
  # interp for plot
  
  # first create new grid
  uni_dts <- sort(unique(dat_in$Date))
  dists <- unique(dat_in$dist)
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
  in_col <- colorRampPalette(cols)
  
  # function to transpose
  rotate <- function(x) t(apply(x, 2, rev))
  
  # plot margins
  plot.new()
  par(new = "TRUE",plt = c(0.1,0.83,0.15,0.9),las = 1,cex.axis = 1)
  
  # contour plot with isolines
  filled.contour3(x = x.val, y = -1 * rev(y.val), z = rotate(z.val),
    color.palette = in_col, ylab = ylab,
    nlevels = ncol, # for smoothed colors
    axes = F)
  contour(x = x.val, y =  -1 * rev(y.val), z = rotate(z.val), nlevels= num_levs,
    axes = F, add = T)
  
  # axis labels
  axis(side = 2, at = -1 * seq(0, 35, by = 5), labels = seq(0, 35, by = 5))
  axis.Date(side = 3, x = as.Date(x.val), format = '%m-%Y')
  axis(side = 1, at = uni_dts, labels = uni_dts, tick = F, cex.axis = 0.5, las = 2, line = -0.5)
  axis(side = 4, at = -1 * rev(dists), labels = rev(unique(dat_in$Station)), tick = F, 
    cex.axis = 0.5, las = 1, line = -0.5)
  box()
  
  # legend
  par(new = "TRUE", plt = c(0.90,0.94,0.15,0.9), las = 1,cex.axis = 1)
  filled.legend(x.val,y.val,rotate(z.val),color=in_col,xlab = "",
    nlevels = num_levs,
    ylab = "",
    ylim = c(min(z.val),max(z.val)))
  
}

######
# plotting functions (not mine)
# used in 'ctd_plot' above
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
{
  # modification by Ian Taylor of the filled.contour function
  # to remove the key and facilitate overplotting with contour()
  # further modified by Carey McGilliard and Bridget Ferris
  # to allow multiple plots on one page

  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
 # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
 # on.exit(par(par.orig))
 # w <- (3 + mar.orig[2]) * par("csi") * 2.54
 # par(las = las)
 # mar <- mar.orig
 plot.new()
 # par(mar=mar)
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                            col = col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}

filled.legend <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
  # modification of filled.contour by Carey McGilliard and Bridget Ferris
  # designed to just plot the legend
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")

    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
}

# the plotting function
# varsel is input name, all dat is input data
plo_fun <- function(varsel, alldat){
  
  toplo <- alldat[, c('datetimestamp', 'stat', varsel)]

  tocomb <- split(toplo, toplo$stat)
  tocomb <- lapply(tocomb, function(x) x[, c('datetimestamp', varsel)])
  
  toplo <- comb(tocomb, date_col = 'datetimestamp', timestep = 30)
  names(toplo)[!names(toplo) %in% 'datetimestamp'] <- names(tocomb)
  
  # make xts
  step <- toplo$datetimestamp
  toplo <- as.matrix(toplo[, -1])
  toplo <- as.xts(toplo, order.by = step)
  
  cols <- as.list(RColorBrewer::brewer.pal(3, 'Set1'))
  names(cols) <- c('P02', 'P05-S', 'P05-B')
  cols <- as.character(cols[names(tocomb)])
  
  dygraph(toplo, ylab = varsel, group = 'group') %>% 
      dyRangeSelector %>% 
      dyOptions(colors = cols)    

}

######
# just a silly function for variable selection
vars <- function(tosel){
 
  wqm <- c('temp', 'pres', 'sal', 'do_mgl', 'turb', 'chla', 'cdom', 'par', 'ATemp', 'BP', 'WSpd')
  ctd <- c('Temp', 'Salinity', 'SigmaT', 'DO', 'DOsat', 'Fluor', 'Turb', 'CDOM')
  
  all <- list(wqm = wqm, ctd = ctd)
  
  return(all[[tosel]])
  
}
