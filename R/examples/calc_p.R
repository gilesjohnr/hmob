library(hmobdata)
data(duration.array.month.level)

# Proportion travellers remaining after 1 generation, where the generation time is 3 days (influenza)
p <- calc.p(d=duration.array.month.level, gen.t=3, n.gen=1)

# Get destination-level mean and variance
p.mean <- apply(p, 2, mean, na.rm=TRUE)
p.var <- apply(p, 2, var, na.rm=TRUE)