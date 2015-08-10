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
  coord_lims = NULL, arrow = 0.2, barmin = 0, barmax = NULL, barcol = NULL, 
  ...){
    
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
  
  # cumulative distance for each station
  sta_lens <- data.frame(
    Station = paste0('P0', seq(1,9)),
    dist = cumsum(
      c(0, 2.588, 2.022, 1.711, 2.571, 3.076, 4.233, 3.656, 3.913))
  )
  
  names(dat.in) <- c('Station', 'Date', 'Layer', 'Depth', 
    'Temp', 'Salinity', 'SigmaT', 'DO', 'DOsat', 'Fluor', 'Turb', 'CDOM', 
    'PAR', 'KPAR', 'N', 'RSQ')
  
  # remove last three columns
  out <- dat.in[, !names(dat.in) %in% c('Layer', 'PAR', 'KPAR', 'N', 'RSQ')]

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
# input 'dat_in' is output from 'form_dat'
# 'var_plo' is variable to plot, e.g., 'Temp'
# 'rngs_in' is output from 'get_rngs' above
# 'num_levs' is number of contours
# 'xlab' and 'ylab' are for axis labels
ctd_plot2 <- function(dat_in, var_plo, rngs_in = NULL, num_levs = 8, ylab = 'Depth (m)',
  xlab = 'Channel distance from P01 to P09 (km)'){

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
  
  # colors for plotting
  in.col <- colorRampPalette(c('tomato', 'lightblue', 'lightgreen','green'))
  
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
  
  # contour plot with isolines
  filled.contour3(x = x.val, y = y.val, z = rotate(z.val),
    color.palette=in.col,
    ylab=ylab, xlab='',
    nlevels=100, # for smoothed colors
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
  axis(side = 3, at = top$dist, labels = top$Station, cex.axis = 0.5,
    tick = F, line = -1)
  
  # masking depth
  poly.x <- approx(x.val, n = nrow(maxd))$y
  with(maxd,
    polygon(
      c(poly.x, rev(poly.x)), 
      c(mllw_m, rep(min(mllw_m), length(mllw_m))), 
      col = alpha('grey', 1)
      ))
  
  ##
  # variable name -lower left
  text(x = par('usr')[1], par('usr')[3] + 1, labels = var_plo, pos = 4, 
    cex = 1.5)
  
  box()
  
  ##
  # legend
  par(new = "TRUE", plt = c(0.87,0.91,0.3,0.9),las = 1,cex.axis = 1)
  filled.legend(x.val,y.val,rotate(z.val),color=in.col,xlab = "",
    nlevels = levs,
    ylab = "",xlim = c(min(xintercepts),max(xintercepts)),
    ylim = c(min(z.val),max(z.val)))
  filled.legend(x.val,y.val,rotate(z.val),color=in.col,xlab = "",
    nlevels = levs,
    ylab = "",xlim = c(min(xintercepts),max(xintercepts)),
    ylim = c(min(z.val),max(z.val)))
  
  }

######
# create contour plots for CTD data, via kriging
# input 'dat_in' is output from 'form_dat'
# 'var_plo' is variable to plot, e.g., 'Temp'
# 'num_levs' is number of contours
ctd_plot <- function(dat_in, var_plo, num_levs = 8){
  
  library(scales)
  library(geoR)
  library(gstat)
  library(reshape2)
  
  # for matrix rotation, otherwise plot is not correct
  rotate <- function(x) t(apply(x, 2, rev))
  
  # number of kriging points as row or column to interp, value is squared 
  num_krig <- 100
  
  # get relevant data from input, convert units for some
  dat_in <- dat_in[, c('Station', 'Depth', var_plo, 'dist')]
  dat_in$Depth <- -1 * dat_in$Depth
  dat_in$dist <- dat_in$dist * 1.6093
  
  # maximum depth of data for each statoin, used for polygon masking
  # value is linearly interpolated to increase samp
  maxd <- aggregate(Depth ~ Station, dat_in, min)$Depth
  maxd <- approx(maxd, n = num_krig)$y
  
  # coords for plot
  x.val <- seq(0, max(dat_in$dist), length = num_krig)
  y.val <- seq(0, min(dat_in$Depth), length = num_krig)
  
  # grid to interpolate
  new_pts <- expand.grid(x.val, y.val)
  
  # kriging values to interpolate
  var.mod <- variogram(list(dat_in[, var_plo]), list(dat_in$dist, dat_in$Depth), 
    cutoff = 40)
  nug <- max(var.mod$gamma)/2  # intercept of sph mod
  rng <- max(var.mod$dist) # distance between points at which semivar asymps
  sil <- max(var.mod$gamma) # semivar value at which semivar asymps, mean works better than max
  
  # get interpolation predictions
  conts <- krige.control(type.krige="ok", cov.model="spherical",
    cov.pars=c(sil, rng), nugget = nug)
  preds <- krige.conv(
    coords = as.matrix(dat_in[,c('dist', 'Depth')]),
    data = dat_in[, var_plo],
    locations = new_pts,
    krige = conts, output = list(messages = F))
  dat_kr <- data.frame(new_pts, val = preds$predict)
  
  # interpolation matrix in format for plotting, rev y.vals
  z.val <- matrix(dat_kr$val, byrow = T, nrow = num_krig)
  y.val <- sort(y.val)
  
  ##
  # start plot
  plot.new()
  
  # number of contours
  levs <- num_levs
  
  in.col <- colorRampPalette(c('green','lightgreen','blue'))
  
  # mask z.val so correct col contours showup
  mask_grd <- sapply(1:num_krig,
    function(x){
      out <- rep(NA, num_krig)
      out[rev(y.val) >= (maxd[x]-0.5)] <- 1
      out}
  )
  z.val <- ifelse(mask_grd, z.val, NA)
  
  # plot margins
  par(new = "TRUE",plt = c(0.15,0.85,0.3,0.9),las = 1,cex.axis = 1)
  
  # contour plot with isolines
  filled.contour3(x = x.val, y = y.val, z = rotate(z.val),
    color.palette=in.col,
    ylab='Depth (m)', xlab='Channel distance from P01 to P09 (km)',
    nlevels=100, # for smoothed colors
    axes = F)
  contour(x = x.val, y = y.val, z = rotate(z.val), nlevels=levs,
    axes = F, add = T)
  
  ##
  # axes
  box()
  
  # x
  axis(side = 1)
  
  # y
  y.axs <- axTicks(2, par('yaxp'))
  axis(side = 2, at = y.axs, labels = abs(y.axs))
  
  # top
  top <- unique(dat_in[, c('Station', 'dist')])
  axis(side = 3, at = top$dist, labels = top$Station, cex.axis = 0.5,
    tick = F, line = -1)
  
  # masking depth
  polygon(c(x.val, rev(x.val)), c(maxd, rep(min(y.val), length(maxd))), 
    col = 'grey')
  
  ##
  # legend
  par(new = "TRUE",plt = c(0.87,0.91,0.3,0.9),las = 1,cex.axis = 1)
  filled.legend(x.val,y.val,rotate(z.val),color=in.col,xlab = "",
    nlevels = levs,
    ylab = "",xlim = c(min(xintercepts),max(xintercepts)),
    ylim = c(min(z.val),max(z.val)))
  filled.legend(x.val,y.val,rotate(z.val),color=in.col,xlab = "",
    nlevels = levs,
    ylab = "",xlim = c(min(xintercepts),max(xintercepts)),
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
