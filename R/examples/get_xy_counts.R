data("d.42") # same as ./BeyondCommuting2/trip_lengths/trip_durations_longform_metadata_42.csv
data("districtIDs") # same as NamNames.csv

orig <- 42 # "Windhoek East"

# trip counts over distance and trip duration for an origin
m <- get.xy.counts(a=orig,               # integer ID of origin district
                   d=d.42,                  # expects "trip_durations_longform2.csv" or similar
                   x.var='distance',
                   y.var='duration',
                   x.int=50,             # distance interval for aggregating distance in km
                   y.int=50,             # duration interval for aggregating duration in days
                   type='matrix',
                   distID=districtIDs,
                   n.cores=4)
see(m)

plot3D::hist3D(x=as.numeric(rownames(m)), y=as.numeric(colnames(m)), z=log(m+1), 
               theta=120, phi=30, scale=F, expand=50,
               xlab='Distance',
               ylab='Duration',
               zlab='Log trip count',
               ticktype="detailed",
               space=0.05, lighting=T, light="diffuse")