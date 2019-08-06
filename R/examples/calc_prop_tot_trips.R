# Longform trip lengths with all metadata attached
d <- read.csv("../SeasonalCommuting/BeyondCommuting2/trip_lengths/trip_durations_longform_metadata.csv", stringsAsFactors=F)

# Proportion of total trips with a duration less than the given values
calc.prop.tot.trips(variable=d$duration, 
                    count=d$count, 
                    vals=c(1,3,7), 
                    type='cumulative')

calc.prop.tot.trips(variable=d$duration, 
                    count=d$count, 
                    vals=c(1,3,7), 
                    type='interval')

# Proportion of total trips with a distance less than the given values in parallel
calc.prop.tot.trips(variable=d$distance, 
                    count=d$count, 
                    vals=c(1:100), 
                    type='cumulative',
                    parallel=TRUE,
                    n.cores=4)

calc.prop.tot.trips(variable=d$distance, 
                    count=d$count, 
                    vals=c(1:100), 
                    type='interval',
                    parallel=TRUE,
                    n.cores=4)
