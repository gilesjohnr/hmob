
load('./output/gravity_model_route_8dists.Rdata')    # mod.grav  #  posterior estimates of gravity model parameters
tmp <- get.param.vals(n.districts=8, name='lambda', level='route', stats=mod.grav)

lambda.hat <- sim.lambda(mu=tmp$mean, sigma=tmp$sd, level='route')