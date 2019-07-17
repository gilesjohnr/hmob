load('./data/duration_data_arrays_5day_full.Rdata')                    # y.month, y.route, y.pop
load('./MARCC/output/decay_model_route_hierarchical_5day_full.Rdata')  # out # Decay model parameters (Lambda)
load('./data/distance_matrix_named.rdata')                             # distance.matrix
pop <- read.csv("./data/population_data.csv")                          # population densiity

districts <- dimnames(y.route)$origin

dens <- vector()
for (i in seq_along(districts)) {
     dens <- c(dens, pop$density[as.numeric(which(pop$ID_2 == districts[i]))])
}

mod.decay <- calc.decay.stats(x=out, 
                              y.route=y.route, 
                              districts=districts, 
                              pop.dens=dens, 
                              dist.mat=distance.matrix[districts, districts])
