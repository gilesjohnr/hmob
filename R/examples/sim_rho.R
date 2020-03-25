d <- read.csv("./data/trip_durations_longform_metadata_13_34_42_85_10_14_16_99_urban_rural.csv", stringsAsFactors=F)
d <- mob.data.array(d=d, time='month', variable='duration', agg.int=1)
p <- calc.prop.remain(d=d, gen.t=3)
rho.hat <- sim.rho(p=p, level='route')