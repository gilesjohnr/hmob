load("./output/TSIR_sim_flu_high.Rdata")

x1 <- sim$B$wait.time
x2 <- sim$R$wait.time

agg1 <- agg.wait.time(x1)
agg2 <- agg.wait.time(x2)

plot(agg1[1,], type='l', col='blue', cex=2)
lines(agg2[1,], col='red', cex=2)