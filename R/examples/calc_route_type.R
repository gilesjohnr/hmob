# Subset of 5 high pop density and 7 low pop density
d <- read.csv("BeyondCommuting2/trip_lengths/trip_durations_longform_metadata_12_dists_hi_lo_dens.csv", stringsAsFactors=F)
pop <- read.csv("./data/population_data.csv")

cutoff <- 980
hi <- pop$ID_2[pop$density >= cutoff]
lo <- pop$ID_2[pop$density < cutoff]

m <- mob.data.array(orig=d$from,
                    dest=d$to,
                    time=d$day, # total trip counts for each unique day
                    count=d$count,
                    name='movement')
str(m)

par(mfrow=c(1,2))

p1 <- calc.route.type(m, hi, lo, per.route=FALSE) # Raw number of trips in each route type

plotrix::violin_plot(p1[,2:5], violin_width = 1, box_width=0.05, 
                     xlab='Route type', ylab='Raw total number trips', main=NULL,
                     x_axis_labels=c('High to high', 'High to low', 'Low to high', 'Low to low'))

p2 <- calc.route.type(m, hi, lo, per.route=TRUE) # Total number of trips per route

plotrix::violin_plot(p2[,2:5], violin_width = 1, box_width=0.05, 
                     xlab='Route type', ylab='Total trips per route', main=NULL,
                     x_axis_labels=c('High to high', 'High to low', 'Low to high', 'Low to low'))






