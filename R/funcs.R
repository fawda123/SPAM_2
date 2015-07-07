# plot adcp data
# dat_in input row to plot
# bins depth bins to plot
# magref maximum scaling value of velecity vector for reference
# lims min, max values for magnitude value
plot_adcp <- function(dat_in, bins = NULL, magref = NULL, lims = c(1, 8)){
  
  browser()
  
  # get bins if not provided
  if(is.null(bins)) 
    bins <- 1:length(grep('^Dir', names(dat_in)))
  
  # subset directions, mags by bins
  dirs <- dat_in[, grep(paste(paste0('^Dir', bins), collapse = '|'), names(dat_in))]
  dirs <- as.numeric(dirs)
  mags <- dat_in[, grep(paste(paste0('^Mag', bins), collapse = '|'), names(dat_in))]
  mags <- as.numeric(mags)
  
  # max magnitue if not provided 
  if(is.null(magref)) magref <- max(mags)
  
  # locations of bins
  yshifts <- cumsum(rep(1/(1 + length(bins)), length(bins)))
  
  # change axis reference for directions
  dirs[dirs > 90] <- 360 - dirs[dirs > 90] + 90
  dirs[dirs <= 90] <- 90 - dirs[dirs <= 90]
  
  # x, y locs from polar coords
  xvals <- mags * cos(pi * dirs / 180)
  yvals <- mags * sin(pi * dirs / 180)
  
  # the plot
  plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE, xlab = '', ylab = '')
  points(rep(0.5, length(bins)), yshifts)
  segments(rep(0.5, max(bins)), yshifts,  0.5 + xvals, yvals + yshifts)
  
}