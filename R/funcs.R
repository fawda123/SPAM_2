# plot adcp data
# dat_in input row to plot
# shp_in SpatialPolygonsDataFrame input for optional map
# loc_in location of ADCP in reference to shp_in
# bins depth bins to plot
# magref maximum scaling value of velocity vector for reference
# lims min, max values for magnitude value
# coord_lims list of constraining x, y locations for plotting shp_in
plot_adcp <- function(dat_in, shp_in = NULL, loc_in = NULL, bins = NULL, magref = NULL, 
  lims = c(0.01, 0.5), coord_lims = NULL){
  
  # get bins if not provided
  if(is.null(bins)) 
    bins <- 1:length(grep('^Dir', names(dat_in)))
  
  # subset directions, mags by bins
  dirs <- dat_in[, grep(paste(paste0('^Dir', bins), collapse = '|'), names(dat_in))]
  dirs <- as.numeric(dirs)
  mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))]
  mags <- as.numeric(mags)
  # mags <- scales::rescale(mags, to = lims)
  
  # max magnitude if not provided 
  if(is.null(magref)) magref <- max(mags)
  
  # change axis reference for directions
  dirs[dirs > 90] <- 360 - dirs[dirs > 90] + 90
  dirs[dirs <= 90] <- 90 - dirs[dirs <= 90]
  
  # x, y locs from polar coords
  xvals <- mags * cos(pi * dirs / 180)
  yvals <- mags * sin(pi * dirs / 180)
  
  # create a simple plot if no shapefile
  if(is.null(shp_in)){
    
    # y locations of bins on plot
    yshifts <- cumsum(rep(1/(1 + length(bins)), length(bins)))
    
    # the plot
    plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE, xlab = '', ylab = '')
    points(rep(0.5, length(bins)), yshifts)
    segments(rep(0.5, max(bins)), yshifts,  0.5 + xvals, yvals + yshifts)
    
  # otherwise plot as facetted shapefile
  } else {
    
    if(is.null(loc_in)) stop('Location needed if shapefile input is used')  
    
    # setup plot data
    vecs <- data.frame(
      long1 = loc_in[1], 
      lat1 = loc_in[2],
      long2 = xvals + loc_in[1], 
      lat2 = yvals + loc_in[2],
      bins = paste('Bin', bins)
      )
    loc_in <- data.frame(long = loc_in[1], lat = loc_in[2])
   
    # plot
    p <- ggplot(loc_in, aes(x = long, y = lat)) + 
      geom_polygon(data = shp_in, aes(x = long, y = lat, group = group), 
        fill = 'lightgrey') +
      geom_point() +
      geom_segment(data = vecs, aes(x = long1, y = lat1, xend = long2, yend = lat2)) +
      theme_classic() + 
      coord_equal() + 
      facet_wrap(~bins) +
      ggtitle(dat_in$datetimestamp)
    
    # constrain plot boundaries
    if(is.null(coord_lims))
      p <- p + coord_map(
        xlim = coord_lims[[1]],
        ylim = coord_lims[[2]]
      )
  
    return(p) 
    
  }
  
}