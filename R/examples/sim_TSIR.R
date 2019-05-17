
d <- read.csv("./BeyondCommuting2/trip_lengths/trip_durations_longform_metadata_13_34_42_85_10_14_16_99_urban_rural.csv", stringsAsFactors=F)
load('./data/distance_matrix_named.rdata')           # distance.matrix
load('./data/N_pop.rdata')                           # N.pop
load('./output/decay_model_8dists.Rdata')            # mod.decay # Decay model parameters (Lambda)
load('./output/gravity_model_route_8dists.Rdata')    # mod.grav  # Gravity model parameters

# lambda.hat
tmp <- get.param.vals(n.districts=8, name='lambda', level='route', stats=mod.grav)
lambda.hat <- sim.lambda(mu=tmp$mean, sigma=tmp$sd, level='route')


# pi.hat
tmp <- get.param.vals(n.districts=8, name='pi', level='route', stats=mod.grav)
pi.hat <- sim.pi(mu=tmp$mean, level='route')


# rho.hat
d <- jags.data.array(d=d, time='month', variable='duration', agg.int=1)
p <- calc.p(d=d, gen.t=3)
rho.hat <- sim.rho(p=p, level='route')

districts <- dimnames(M)$origin
N <- round(N.pop$N[N.pop$ID %in% as.integer(districts)])
I.0 <- rep(0, length(districts))
I.0[6] <- 10

test <- sim.TSIR(districts=districts,  
                 N=N,                        
                 lambda=lambda.hat,           
                 pi=pi.hat,                     
                 rho=rho.hat,                      
                 beta=0.5,                    
                 gamma=1/4,                    
                 gen.t=3,                     
                 max.t=ceiling(365/3),             
                 I.0=I.0,                     
                 freq.dep=TRUE              
)

test[,,1:5] # start
test[,,118:122] # finish