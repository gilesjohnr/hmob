prm <- get.gamma.params(mu=15.3, sigma=9.3)

prm[1]/prm[2] # mean
sqrt(prm[1]/(prm[2]^2)) # sd

plot(density(rgamma(n=1000, shape=prm[1], rate=prm[2])), main='')
abline(v=15.3, lty=2, col='red')

prm <- get.gamma.params(mu=0.65, sigma=0.01)

curve(dgamma(x, prm[1], prm[2]), 0, 1) # prior for shape parameter
abline(v=0.65, lty=2, col='red')