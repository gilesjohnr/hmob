
d <- read.csv("./data/trip_durations_longform_metadata_13_34_42_85_10_14_16_99_urban_rural.csv", stringsAsFactors=F)
load('./data/distance_matrix_named.rdata')                         # distance.matrix
load('./data/N_pop.rdata')                                         # N.pop
load('./output/decay_model_route_12dists.Rdata')                   # mod.decay # Decay model parameters (Lambda)
load('./output/gravity_model_route_12dists.Rdata')                 # mod.grav  # Gravity model with duration
load('./output/gravity_model_basic_route_12dists.Rdata')           # mod.grav.basic  # Basic gravity model
load('./data/prop_leave.Rdata')                                    # prop.leave # observed proportion individuals leaving origin at time t in trip duration data  
load('./output/prop_remaining.Rdata')                              # p # observed proportion of individuals remaining for full pathogen generation

districts <- dimnames(M)$origin
n.districts <- length(districts)
N <- round(N.pop$N[N.pop$ID %in% as.integer(districts)])
I.0 <- rep(0, length(districts))
I.0[2] <- 1

# lambda.hat
tmp <- get.param.vals(n.districts=n.districts, name='lambda', level='route', stats=mod.grav)
lambda.hat <- sim.lambda(mu=tmp$mean, sigma=tmp$sd, level='route')

# pi.hat
tmp <- get.param.vals(n.districts=n.districts, name='pi', level='route', stats=mod.grav)
pi.hat <- sim.pi(mu=tmp$mean, level='route')

# pi.hat.basic
tmp <- get.param.vals(n.districts=n.districts, name='pi', level='route', stats=mod.grav.basic)
pi.hat.basic <- sim.pi(mu=tmp$mean, level='route')

# rho.hat
d <- jags.data.array(d=d, time='month', variable='duration', agg.int=1)
p <- calc.p(d=d, gen.t=3)
rho.hat <- sim.rho(p=p, level='route')

# tau.hat
tau.hat <- sim.tau(prop.leave)
tau.hat <- tau.hat[names(tau.hat) %in% districts]

# Simulate measles epidemic using basic gravity model
grav <- sim.TSIR(districts=districts,                 
                 N=N,                                 
                 pi=pi.hat.basic,        # connectivity comes from basic gravity model formulation 
                 tau=tau.hat,            # Probability of leaving district i
                 beta=0.7125,            # Transmission rate
                 gamma=1/21,             # Recovery rate                         
                 gen.t=gen.t,                         
                 max.t=max.t,                         
                 I.0=I.0,                             
                 duration=F,             # do NOT use trip duration in force of infection                 
                 freq.dep=T                           
)

grav[,,1:5]

# Simulate influenza epidemic using gravity model with duration
grav.dur <- sim.TSIR(districts=districts,                 
                     N=N,                                 
                     tau=tau.hat,            # Probability of leaving district i
                     lambda=lambda.hat,      # Matrix of trip duration decay for route i to j 
                     pi=pi.hat,              # Matrix of district connectivity (estimated pi_ijt)
                     rho=rho.hat,            # Matrix of proportion of travellers remaining for full generation when moving from i to j
                     beta=0.5,               # Transmission rate
                     gamma=1/14,             # Recovery rate                          
                     gen.t=gen.t,                         
                     max.t=max.t,                
                     I.0=I.0,                             
                     duration=T,             # Use trip duration in force of infection
                     freq.dep=T                         
)

grav.dur[,,1:5]