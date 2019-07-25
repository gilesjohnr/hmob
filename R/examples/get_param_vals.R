load('./output/decay_1day_62dists_summary.Rdata')  # mod.decay # Summary of decay model parameters (Lambda)

# Get trip duration decay rate rate parameter lambda
lam <- get.param.vals(n.districts=62, 
                      name='lambda',
                      level='route',
                      stats=mod.decay,
                      n.cores=4)

see(lam$mean)
