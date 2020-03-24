# Duration data for the purpose of subsetting districts
y.route <- load.obj(3, './data/duration_data_arrays_1day_full.Rdata')      # y.month, y.route
y.route <- get.subsamp(y.route, min.locations=30, min.samp=20)

D <- load.obj(1, './data/distance_matrix_named.rdata')                # distance.matrix
N <- load.obj(1, './data/N_pop.rdata')  
B <- load.obj(2, './output/gravity_model_basic_nogamma.Rdata')
R <- load.obj(2, './output/gravity_model_duration_multialpha_nogamma_meanbasic.Rdata')

load('./output/decay_1day_62dists_summary.Rdata')                   # mod.decay # Decay model parameters (Lambda) 
load('./output/prop_remain_6pathogens_62dists_subsamp100.Rdata')    # prop.remain # proportion individuals remaining for full generation for each pathogen generation
load('./data/prop_leave.rdata')                                     # prop.leave # observed proportion individuals leaving origin at time t in trip duration data  

districts.all <- dimnames(D)[[1]] # full set of district names
districts <- attributes(y.route)$dimnames$origin # subset of 62 districts
n.districts <- length(districts)

tmp <- districts.all %in% districts
D <- D[tmp, tmp]
N <- N[names(N) %in% districts]
prop.leave <- prop.leave[,dimnames(prop.leave)$origin %in% districts]
lambda <- get.param.vals(n.districts=n.districts, name='lambda', level='route', stats=mod.decay)

params <- list(
     influenza=data.frame(pathogen='influenza', beta=1.5, gamma=0.75, gen=3, yrs=0.6)
)

pathogen <- 'influenza'
intro.district <- 42

I.0 <- rep(0, n.districts)
I.0[which(districts == intro.district)] <- 1   # introduction  

t <- Sys.time()
sim <- sim.TSIR.full(
     N=N,                                                                  # Vector giving the population size of each district
     D=D,                                                                  # Distance matrix
     lambda=lambda,                                                        # Decay model parameters (Lambda)
     B=B,                                                                  # Gravity model with duration
     R=R,                                                                  # Basic gravity model
     prop.leave=prop.leave,                                                # observed proportion individuals leaving origin at time t in trip duration data 
     prop.remain=prop.remain[[which(names(prop.remain) == pathogen)]],     # bserved proportion of individuals remaining in destination j
     beta=params[[pathogen]]$beta,                                         # Transmission rate
     gamma=params[[pathogen]]$gamma,                                       # Recovery rate
     gen=params[[pathogen]]$gen,                                           # Pathogen generation time
     I.0=I.0,                                                              # Vector giving number of infected individuals in each district at time 0
     N.sim1=5,                                                             # Number of times to simulate matrices of model parameters (lambda, pi, tau, rho)
     N.sim2=5,                                                             # Number of times to simulate epidemic outcomes under each realization of model parameters
     max.t=ceiling((365*params[[pathogen]]$yrs)/params[[pathogen]]$gen),   # Maximum number of generations
     parallel=TRUE,
     n.cores=n.cores
)
Sys.time() - t

