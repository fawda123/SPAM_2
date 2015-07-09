######
# plot adcp data
# dat_in input row to plot
# shp_in SpatialPolygonsDataFrame input for optional map
# loc_in location of ADCP in reference to shp_in
# bins depth bins to plot
# lims min, max values for magnitude value
# coord_lims list of constraining x, y locations for plotting shp_in
# arrow length of arrow ends
plot_adcp <- function(dat_in, shp_in = NULL, loc_in = NULL, bins = NULL,
  lims = c(0.01, 0.5), coord_lims = NULL, arrow = 0.2, barmax = 0.3){
    
  if(is.null(loc_in)) stop('ADCP location needed')  
  
  # get bins if not provided
  if(is.null(bins)) 
    bins <- 1:length(grep('^Dir', names(dat_in)))
  
  # subset directions, mags by bins
  dirs <- dat_in[, grep(paste(paste0('^Dir', bins), collapse = '|'), names(dat_in))]
  dirs <- as.numeric(dirs)
  mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))]
  mags <- as.numeric(mags)

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
    geom_point() +
    geom_segment(data = vecs, aes(x = long1, y = lat1, xend = long2, yend = lat2), 
      arrow = grid::arrow(length = grid::unit(arrow, "cm"))
      ) +
    theme_classic() + 
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
    geom_bar(stat = 'identity') + 
    facet_wrap(~bins, ncol = 1) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(), 
      axis.text.y = element_text(size = 4)
      ) + 
    scale_y_continuous(limits = c(0, barmax)) + 
    coord_flip() +
    ylab('Velocity')

  # combined grob
  gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(1, 1), 
    main = as.character(dat_in$datetimestamp))
 
}

######
# 
# dat_in input row to plot
# shp_in SpatialPolygonsDataFrame input for optional map
# loc_in location of ADCP in reference to shp_in
# ... other arguments passed to plot_adcp
get_multi <- function(dat_in, shp_in = NULL, loc_in = NULL, interp = NULL, ...){
  
  # remove depth column
  dat_in <- dat_in[, grep('^Depth', names(dat_in), invert = TRUE)]
  
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

  
  # pass each row to plot_adcp
  for(row in 1:nrow(dat_in)){
    
    # console output
    perc <- 5 * which(row == counts)
    if(length(perc) != 0) cat(perc, '\t')
    
    # create plot
    plot_adcp(dat_in[row, ], shp_in, loc_in, ...)
    
  }
  
}
