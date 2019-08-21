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

p <- calc.prop.route.type(m, hi, lo)

boxplot.matrix(as.matrix(p[,2:5]), col='lightblue')

cutoff <- 100
hi <- pop$ID_2[pop$density >= cutoff]
lo <- pop$ID_2[pop$density < cutoff]

p <- calc.prop.route.type(m, hi, lo)

boxplot.matrix(as.matrix(p[,2:5]), col='lightblue')


