sims <- c(
     "./output/TSIR_sim_influenza_high.Rdata",
     "./output/TSIR_sim_influenza_low.Rdata",
     "./output/TSIR_sim_measles_high.Rdata",
     "./output/TSIR_sim_measles_low.Rdata",
     "./output/TSIR_sim_malaria_high.Rdata",
     "./output/TSIR_sim_malaria_low.Rdata",
     "./output/TSIR_sim_ebola_high.Rdata",
     "./output/TSIR_sim_ebola_low.Rdata",
     "./output/TSIR_sim_pertussis_high.Rdata",
     "./output/TSIR_sim_pertussis_low.Rdata",
     "./output/TSIR_sim_sars_high.Rdata",
     "./output/TSIR_sim_sars_lowRdata"
)

# Calculate peak waiting time and proportion infected for each scenario and district and combine
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)
parallel::clusterExport(cl, ls(environment()), envir=environment())

sims <- foreach (i=1:length(sims), .combine='rbind', .packages=c('stringr', 'hmob')) %dopar% {
     
     sim <- load.obj(sims[i])
     sim.names <- str_split(str_sub(sims[i], end=-7), "_", simplify=TRUE)
     calc.timing.magnitude(sim=sim, N=N.pop, pathogen=sim.names[1,3], intro=sim.names[1,4])
}

# Add waiting time in days
sims$days <- NA
sims$days[sims$pathogen == 'influenza'] <- sims$wait.time[sims$pathogen == 'influenza']*3
sims$days[sims$pathogen == 'measles'] <- sims$wait.time[sims$pathogen == 'measles']*14
sims$days[sims$pathogen == 'malaria'] <- sims$wait.time[sims$pathogen == 'malaria']*60
sims$days[sims$pathogen == 'ebola'] <- sims$wait.time[sims$pathogen == 'ebola']*16.6
sims$days[sims$pathogen == 'pertussis'] <- sims$wait.time[sims$pathogen == 'pertussis']*25
sims$days[sims$pathogen == 'sars'] <- sims$wait.time[sims$pathogen == 'sars']*8
