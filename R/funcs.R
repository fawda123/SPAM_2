######
# plot adcp data
# dat_in input row to plot
# all_in all adcp data
# shp_in SpatialPolygonsDataFrame input for map
# loc_in location of ADCP in reference to shp_in
# bins depth bins to plot
# coord_lims list of constraining x, y locations for plotting shp_in
# arrow end of arrow size
# barmin lower axis limit on barplot
# barmax upper axis limit on barplot
# barcol vector of colors for bars on barplot
plot_adcp <- function(dat_in, all_in, shp_in, loc_in, bins = 1:6, 
  coord_lims = NULL, arrow = 0.2, barmin = 0, barmax = NULL, barcol = NULL){
    
  # subset directions, mags by bins
  dirs <- dat_in[, grep(paste(paste0('^Dir', bins), collapse = '|'), names(dat_in))]
  dirs <- as.numeric(dirs)
  mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))]
  mags <- as.numeric(mags)

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
  xvals <- 0.5 * cos(pi * dirs / 180) # mags * cos(pi * dirs / 180)
  yvals <- 0.5 * sin(pi * dirs / 180) # mags * sin(pi * dirs / 180)
  
  # setup plot data
  vecs <- data.frame(
    long1 = loc_in[1], 
    lat1 = loc_in[2],
    long2 = xvals + loc_in[1], 
    lat2 = yvals + loc_in[2],
    bins = paste('Bin', bins),
    mags = mags
    )
  loc_in <- data.frame(long = loc_in[1], lat = loc_in[2])
  
  # dir plot
  p1 <- suppressMessages({ggplot(loc_in, aes(x = long, y = lat)) + 
    coord_fixed(xlim = bbox(shp_in)[1, ], ylim = bbox(shp_in)[2, ]) + 
    geom_polygon(data = shp_in, aes(x = long, y = lat, group = group), 
      fill = 'lightgrey') +
    geom_segment(data = vecs, aes(x = long1, y = lat1, xend = long2, yend = lat2, 
      colour = bins), size = 1.5, alpha = 0.6) +
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
  p2 <- ggplot(vecs, aes(x = gsub('Bin ', '', bins), y = mags)) + 
    geom_bar(stat = 'identity', aes(fill = bins, colour = bins)) + 
    facet_wrap(~bins, ncol = 1) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(), 
      axis.text.y = element_text(size = 4), 
      legend.position = 'none'
      ) + 
    scale_y_continuous(limits = c(barmin, barmax)) + 
    scale_fill_manual(values = as.character(barcol)) + 
    scale_colour_manual(values = as.character(barcol)) +
    coord_flip() +
    ylab('Velocity')

  # pressure plot
  p3 <- plot_press(dat_in, all_in) 
  p3 <- p3 + theme(plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0.4), "in"))
  
  # combined grob
  gridExtra::grid.arrange(
    p3, 
    gridExtra::arrangeGrob(p1, p2, ncol = 2, widths = c(1, 1)), 
    ncol = 1,
    main = as.character(dat_in$datetimestamp), 
    heights = c(0.2, 1)
  )
 
}

######
# plot the pressure time series within a window
#
# dat_in row of adcp data at center of plot
# all_in all adcp data
# win window around center of plot, default two days
plot_press <- function(dat_in, all_in, win = NULL){

  # window defaults to one day
  if(is.null(win))
    win <- 60 * 60 * 24
  
  # x limits of plot from window
  lims <- with(dat_in, c(datetimestamp - win, datetimestamp + win))
  
  # the plot and return
  p1 <- ggplot(dat_in, aes(x = datetimestamp, y = Depth.mm./1000)) +
    geom_line(data = all_in, aes(x = datetimestamp, y = Depth.mm./1000)) +
    geom_point(colour = 'lightgreen', size = 4) +
    scale_x_datetime('Time stamp', limits = lims) +
    scale_y_continuous('Depth (m)') +
    theme_classic()
  
  return(p1)
  
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

