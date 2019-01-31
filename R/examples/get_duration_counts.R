data("d.42") # same as ./BeyondCommuting2/trip_lengths/trip_durations_longform_metadata_42.csv
data("districtIDs") # same as NamNames.csv
data("admin2")

orig <- 42 # "Windhoek East"
dest <- 16

# Longform dataframe 
df <- get.duration.counts(a=orig, b=dest, t='month', d=d.42, type='dataframe', n.cores=4)
head(df)

# Same data in matrix format
m <- get.duration.counts(a=orig,              # integer ID of origin district
                         b=dest,              # integer ID of destination district
                         t='month',           # temporal interval (e.g. 'month', 'week', 'doy')
                         d=d.42,              
                         type='matrix',       # return a matrix or long-form dataframe
                         distID=districtIDs,  
                         n.cores=NULL)        # number of cores to run parallel, if NULL, uses half
see(m)

par(mfrow=c(1,2))
trip.map(a=orig, b=dest)
plot3D::hist3D(x=1:nrow(m), y=1:30, z=m[,1:30],
              theta=130, phi=20, scale=F, expand=0.01,
              xlab='Month',
              ylab='Trip duration',
              zlab='Trip count',
              ticktype="detailed",
              space=0.1, lighting=T, light="diffuse", shade = 0.75)
title(get.names(orig, dest), outer=T, line=-3, cex=2)