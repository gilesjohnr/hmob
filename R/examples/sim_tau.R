load('./data/leave_data.Rdata')
load('./data/stay_data.Rdata')

prop.leave <- leave/stay

tau.hat <- sim.tau(prop.leave)
head(tau.hat)