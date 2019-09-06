# Route-level distance matrix
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    variable=df$distance,
                    agg.int=10, # count trips per 10km intervals
                    name='distance')

# Mean trip counts for the whole population at each 10km distance interval
m2 <- mob.data.array.pop.level(m, 'distance')


# Month-level aggregated duration matrix 
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    time=df$month,
                    count=df$count,
                    variable=df$duration,
                    agg.int=3, # aggregated to a generation time of 3 days
                    name='duration')
str(m)

# Mean trip duration count for the whole population
m2 <- mob.data.array.pop.level(m, 'duration')
