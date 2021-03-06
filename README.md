
## README

`adcp_dat.RData`: ADCP data from P05

`adcp_datP.RData`: reprocessed version of `acdp_dat.RData` that has bins 1 - 3 averaged, then rotated along first principal component axis.  Columns are datetimestamp, MagN (averaged north vector), MagE (averaged east vector), Dir (0-360 direction based on averaged MagN, MagE), Mag (magnitude/speed of averaged MagN, MagE along Dir), DirP (direction of the first principal component axis from Dir/Mag), and MagP (magnitude/speed along the first principal component axis

`ctd_dat.RDAta`: CTD monthly casts along tidal axis

`depth_tran.RData`: metadata for depth transect along tidal axis of Escambia bay, distance and depth

`met_dat.RData`: metabolism estimates for P02, P05-S, P05-B

`pbay.RData`: spatial polygon data frame for Pensacola Bay

`wqm_dat.RData`: data from water quality monitors at P02, P05-S, PO5-B

`wx_dat.RData`: supplementary weather data from PNS
