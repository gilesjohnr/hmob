# Month-level movement matrix
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    time=df$month,
                    count=df$count,
                    name='movement')
str(m)

# Mean-monthly movement for each route
m2 <- mob.data.array.pop.level(m, 'movement')


# Route-level distance matrix
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    count=df$count,
                    variable=df$distance,
                    agg.int=10, # count trips per 10km intervals
                    name='distance')

# Mean trip counts at each 10km distance interval for each origin
m2 <- mob.data.array.route.level(m, 'distance')


# Month-level aggregated duration matrix 
m <- mob.data.array(orig=df$from,
                    dest=df$to,
                    time=df$month,
                    count=df$count,
                    variable=df$duration,
                    agg.int=3, # aggregated to a generation time of 3 days
                    name='duration')
str(m)

# Mean monthly trip duration count for each route
m2 <- mob.data.array.route.level(m, 'duration')