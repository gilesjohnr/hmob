load("./output/TSIR_sim_flu_high.Rdata")

x <- sim$B$wait.time   # basic gravity model
#x <- sim$R$wait.time   # gravity model with duration 

agg <- agg.wait.time(x)
hpd <- calc.hpd(agg)

par(mfrow=c(3,4))
for (i in 1:nrow(hpd)) {
     plot(agg1[i,], type='l', main=paste("district ", hpd$district[i]), ylab='Waiting time density')
     abline(v=hpd$lo95[i], lty=2, col='red')
     abline(v=hpd$hi95[i], lty=2, col='red')
     abline(v=hpd$max[i], col='red')
}

