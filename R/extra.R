library(ggplot2)
library(dplyr)
library(tidyr)

data(adcp_dat)

 
all_in <- adcp_dat
bins <- 1
val <- 39
rng <- with(all_in, c(datetimestamp[val], datetimestamp[val + 12]))
  

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
toplo <- full_join(toplo_mag, toplo_dir, by = c('datetimestamp', 'bin'))

# change axis reference for directions
toplo$dirs[toplo$dirs > 90] <- with(toplo, 360 - dirs[dirs > 90] + 90)
toplo$dirs[toplo$dirs <= 90] <- with(toplo, 90 - dirs[dirs <= 90])

# x, y locs from polar coords
xvals <- with(toplo, mags * cos(pi * dirs / 180))
yvals <- with(toplo, mags * sin(pi * dirs / 180))


xvals1 <- 0
yvals1 <- 0
xvals2 <- xvals1 + xvals
yvals2 <- yvals
plot(c(-0.06, 0.06), c(-0.06, 0.06), type = 'n')
points(0, 0)
segments(xvals1, yvals1, xvals2, yvals2) 

# angle to reproject
theta <- 5

# get diff of angle from vector and 180 plus vector
diffval <- with(toplo, c(dirs - theta))

# get magnitude of new vectors
magsrot <- with(toplo, mags * cos(pi * diffval/180))

# find values in polar coords given angle and mag
xrot <- magsrot * cos(pi * theta/180)
yrot <- magsrot * sin(pi * theta/180)
  
segments(xvals1, yvals1, xrot, yrot, col = 'blue') 
  