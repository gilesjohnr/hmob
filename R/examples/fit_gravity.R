M <- load.obj(2, './data/movement_data_arrays_full.Rdata')            # M.route
D <- load.obj(1, './data/distance_matrix_named.rdata')                # distance.matrix
N <- load.obj(1, './data/N_pop.rdata')                                # N.pop

M <- M[1:5, 1:5]

districts <- attributes(M)$dimnames$origin 
D <- D[districts, districts]
N <- N[names(N) %in% districts]

out <- fit.gravity(M=M,
                   D=D,
                   N=N,
                   n.chain=2,
                   n.adapt=1000,
                   n.burn=4000,
                   n.samp=4000,
                   n.thin=1,
                   parallel=F)

summary(out)

