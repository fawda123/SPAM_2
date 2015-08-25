source('R/funcs.R')

library(dplyr)
library(tidyr)
library(SWMPr)

data(adcp_dat)
data(pbay)

# rotate magnitude of each vector north/south (positive is north)
rots <- vecrots(adcp_dat)
tmp <- rots[, grepl('^Depth.mm.$|^datetimestamp$|^Mag', names(rots))] %>% 
  mutate(Depth.mm. = c(NA, diff(Depth.mm.))) %>% 
  rename(del_dep = Depth.mm.) %>% 
  gather('bin', 'val', -del_dep, -datetimestamp) %>% 
  mutate(bin = gsub('^Mag', 'Bin ', bin)) %>% 
  filter(del_dep > -200 & del_dep < 190) # remove outliers

ggplot(tmp, aes(x = del_dep, y = val)) +
  geom_point() +
  facet_wrap(~ bin, ncol = 4) +
  theme_bw() + 
  scale_x_continuous('Change in height (m)') + 
  scale_y_continuous('North vector (m/s)') +
  stat_smooth(method = 'lm', se = F)
  
dist <- vecdist(rots, sepout = T)

# plot cumulative distances by bin
toplo <- dist[['cdis']] %>% 
  mutate(cDisnm = gsub('cDis', 'Bin', cDisnm))

p1 <- ggplot(toplo, aes(x = datetimestamp, y = cDis)) + 
  geom_point(size = 0.5) + 
  facet_wrap(~ cDisnm, ncol = 1, scales = 'free_y') +
  theme_bw() +
  scale_y_continuous('Cumulative distance (m)')

toplo2 <- filter(toplo, datetimestamp <= datetimestamp[300] & datetimestamp >= datetimestamp[100])

p2 <- ggplot(toplo2, aes(x = datetimestamp, y = cDis)) + 
  geom_line() + 
  facet_wrap(~ cDisnm, ncol = 1, scales = 'free_y') +
  theme_bw()  +
  scale_y_continuous('Cumulative distance (m)')

###
# delta x/delta d related to do

delxt <- vecdist(rots, sepout = T)[[1]] %>% 
  group_by(Disnm) %>% 
  mutate(delxt = c(NA, diff(Dis))) %>% 
  select(-Dis) %>% 
  spread(Disnm, delxt) %>% 
  data.frame
  
# load wqm_dat, get P05B
data(wqm_dat)

do_ts <- filter(wqm_dat, stat == 'P05-B') %>% 
  select(datetimestamp, do_mgl) %>% 
  na.omit

dat <- comb(do_ts, delxt, date_col = 'datetimestamp', timestep = 120, method = 2) %>% 
  mutate(
    deldo = c(diff(do_mgl), NA)
    ) %>% 
  gather('bin', 'val', -datetimestamp, -do_mgl, -deldo) %>% 
  mutate(bin = gsub('^Dis', 'Bin', bin))

p3 <- ggplot(dat, aes(x = val, y = deldo)) +
  geom_point() + 
  facet_wrap(~ bin, scales = 'free') +
  scale_y_continuous('Delta DO / Delta t (mg/L per two hours)') +
  scale_x_continuous('Delta X / Delta t (m per two hours)') + 
  theme_bw() +
  stat_smooth(method = 'lm', se = F) + 
  ggtitle('P05-B')

group_by(dat, bin) %>% 
  summarise(cors = cor(val, deldo, use = 'complete'))

##
# surface data

delxt <- vecdist(rots, sepout = T)[[1]] %>% 
  group_by(Disnm) %>% 
  mutate(delxt = c(NA, diff(Dis))) %>% 
  select(-Dis) %>% 
  spread(Disnm, delxt) %>% 
  data.frame
  
# load wqm_dat, get P05B
data(wqm_dat)

do_ts <- filter(wqm_dat, stat == 'P05-S') %>% 
  select(datetimestamp, do_mgl) %>% 
  na.omit

dat <- comb(do_ts, delxt, date_col = 'datetimestamp', timestep = 120, method = 2) %>% 
  mutate(
    deldo = c(diff(do_mgl), NA)
    ) %>% 
  gather('bin', 'val', -datetimestamp, -do_mgl, -deldo) %>% 
  mutate(bin = gsub('^Dis', 'Bin', bin))

p4 <- ggplot(dat, aes(x = val, y = deldo)) +
  geom_point() + 
  facet_wrap(~ bin, scales = 'free') +
  scale_y_continuous('Delta DO / Delta t (mg/L per two hours)') +
  scale_x_continuous('Delta X / Delta t (m per two hours)') + 
  theme_bw() +
  stat_smooth(method = 'lm', se = F) +
  ggtitle('P05-S')

group_by(dat, bin) %>% 
  summarise(cors = cor(val, deldo, use = 'complete'))

pdf('C:/Users/mbeck/Desktop/adcp_exp.pdf', family = 'serif')
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()
