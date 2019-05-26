load('./data/N_pop.rdata')                                         # N.pop
load('./output/decay_model_route_12dists.rdata')                   # mod.decay # Decay model parameters (Lambda)
load('./output/gravity_model_route_12dists.rdata')                 # mod.grav, M # Gravity model with duration
load('./output/gravity_model_basic_route_12dists.rdata')           # mod.grav.basic  # Basic gravity model
load('./data/prop_leave.rdata')                                    # prop.leave # observed proportion individuals leaving origin at time t in trip duration data  
load('./output/prop_remaining.rdata')                              # p # observed proportion of individuals

districts <- dimnames(M)$origin
N <- round(N.pop$N[N.pop$ID %in% as.integer(districts)])
n.districts <- length(districts)

lambda <- get.param.vals(n.districts=n.districts, name='lambda_1', level='route', stats=mod.decay)
pi.duration <- get.param.vals(n.districts=n.districts, name='pi', level='route', stats=mod.grav)
pi.basic <- get.param.vals(n.districts=n.districts, name='pi', level='route', stats=mod.grav.basic)

pathogen <- 'flu'
introduction <- 'high'

beta <- 1.5                             # Transmission rate
gamma <- 0.75                           # Recovery rate
gen <- 3                                # Generation time of the pathogen in days
yrs <- 1                                # Numerber of years to run simulation
max.t <- ceiling((365*yrs)/gen)

I.0 <- rep(0, length(districts))
if (introduction == 'high') {
     
     I.0[which(districts == '13')] <- 1   # high density introduction  
     
} else if (introduction  == 'low') {
     
     I.0[which(districts == '14')] <- 1  # low density introduction
}

t <- Sys.time()
tmp <- sim.TSIR.full(
     districts=districts,         # Vector of district names
     N=N,                         # Vector giving the population size of each district
     lambda=lambda,               # Decay model parameters (Lambda)
     pi.basic=pi.basic,           # Gravity model with duration
     pi.duration=pi.duration,     # Basic gravity model
     prop.leave=prop.leave,       # observed proportion individuals leaving origin at time t in trip duration data 
     prop.remain=p,               # observed proportion of individuals remaining in destination j
     beta=beta,                   # Transmission rate
     gamma=gamma,                 # Recovery rate
     gen=gen,                     # Pathogen generation time
     I.0=I.0,                     # Vector giving number of infected individuals in each district at time 0
     N.sim1=10,                   # Number of times to simulate matrices of model parameters (lambda, pi, tau, rho)
     N.sim2=10,                   # Number of times to simulate epidemic outcomes under each realization of model parameters
     max.t=max.t,                 # Maximum number of generations
     n.cores=4
)
Sys.time() - t

save(tmp, file=paste("./output/TSIR_sim_", pathogen, "_", introduction, ".Rdata", sep=""))
