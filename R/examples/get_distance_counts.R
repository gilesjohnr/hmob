# Distribution of trip distances that originate from Windhoek East (42)

data(d.42) # same as ./BeyondCommuting2/trip_lengths/trip_durations_longform_metadata_42.csv
data(districtIDs) # same as NamNames.csv
orig <- "Windhoek East"

m <- get.distance.counts(a=orig, t='month', d=d.42, n.cores=4)

see(m)

lattice::wireframe(m[,1:50],
                   xlab = "Month", ylab = "Distance", zlab='Freq',
                   drape = T, colorkey = F,
                   light.source = c(10,0,10), 
                   col.regions = colorRampPalette(c("blue", "red"))(100),
                   screen = list(z = 230, x = -60))