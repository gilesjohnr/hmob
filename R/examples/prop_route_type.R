d <- read.csv("BeyondCommuting2/trip_lengths/trip_durations_longform_metadata_12_dists_hi_lo_dens.csv", stringsAsFactors=F)

test <- calc.prop.route.type(d=d, 
                             hi=c(42,13,3,87,34), 
                             lo=c(10,15,14,48,29,103,64))

boxplot.matrix(as.matrix(test[,2:5]), col='lightblue')

