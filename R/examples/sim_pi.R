load('./output/gravity_model_8dists.Rdata')          # Gravity model parameters
tmp <- get.param.vals(n.districts=8, name='pi', level='month', stats=s.grav)
pi.hat <- sim.connectivity(mu=tmp$mean)