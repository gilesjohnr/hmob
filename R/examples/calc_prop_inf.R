load("./output/TSIR_sim_influenza_high.Rdata") # sim
load('./data/N_pop.rdata')                     # N.pop

# Get only relevant districts
districts <- colnames(sim$B$tot.inf)
N <- N[which(N$ID %in% districts), 'N']

# Calculate mean proportion infected in each district
prop.inf.basic <- calc.prop.inf(sim$B$tot.inf, N=N)
prop.inf.duration <- calc.prop.inf(sim$R$tot.inf, N=N)