# Subset of 5 high pop density and 7 low pop density
df <- read.csv("BeyondCommuting2/trip_lengths/trip_durations_longform_metadata_12_dists_hi_lo_dens.csv", stringsAsFactors=F)

# Simple route-level movement matrix
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    name='movement')
str(m)

# Week-level movement matrix
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    time=df$week,
                    count=df$count,
                    name='movement')
str(m)

# Route-level distance matrix
# Counts total trips that occur with distance intervals of width agg.int km
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    variable=df$distance,
                    name='distance') # no aggregation so trips represent counts per each 1 km
str(m)

m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    variable=df$distance,
                    agg.int=5, # count trips per 5km intervals
                    name='distance')
str(m)

# Month-level aggregated distance matrix
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    time=df$month,
                    variable=df$distance,
                    agg.int=5, # count trips per 5km intervals
                    name='distance')
str(m)

# Route-level duration matrix
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    variable=df$duration,
                    name='duration') # no aggregation so counts represent total trips for all durations in 1 day increments
str(m)

# Month-level aggregated duration matrix 
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    time=df$month,
                    count=df$count,
                    variable=df$duration,
                    agg.int=3, # aggregated to a generation time of 3 days
                    name='duration')
str(m)

# Total number trips leaving each origin across all times
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    name='leave')
str(m)

# Total number trips leaving each origin on each day of the year
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    time=df$doy,
                    count=df$count,
                    name='leave')
str(m)