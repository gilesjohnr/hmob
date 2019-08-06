
# Duration data for the purpose of subsetting districts
load('./data/duration_data_arrays_1day_full.Rdata') 

# Proportion travellers remaining for full epidemic generation based on subsample of 10, where the generation time is 14 days (measles)
p1 <- calc.prop.remain(d=y.route, gen.t=14, sub.samp=10)
p2 <- calc.prop.remain(d=y.month, gen.t=14, sub.samp=10)

# Get destination-level mean and variance
p1.mean <- apply(p1, 2, mean, na.rm=TRUE)
p1.var <- apply(p1, 2, var, na.rm=TRUE)

p2.mean <- apply(p2, 2, mean, na.rm=TRUE)
p2.var <- apply(p2, 2, var, na.rm=TRUE)
