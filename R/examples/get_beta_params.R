### Analytic solution
mu <- 0.6
sigma <- 0.005
prm <- get.beta.params(mu=mu, sigma=sigma) 
curve(dbeta(x, prm$shape1, prm$shape2), 0, 1, lwd=2, xlab="Random variable", ylab='Probability density')
abline(v=mu, col='blue')
abline(v=c(mu-sqrt(sigma), mu+sqrt(sigma)), col='blue', lty=2)


### Numeric solution using SSE
quantiles <- c(0.001, 0.25, 0.5, 0.75, 0.999)
probs1 <- c(0.26, 0.57, 0.73, 0.81, 0.95) # WHO region AFR, <12mos, n=16
probs2 <- c(0.86, 0.86, 0.88, 0.92, 0.94) # WHO region AFR, >12mos, n=4

v <- rbind(probs1, probs2)
dimnames(v) <- list(Age=c('< 12mos', '> 12mos'), 
                    Quantiles=c('min', '25th', 'median', '75th', 'max'))

par(mfrow=c(1,2))
prm <- get.beta.params(quantiles=quantiles, probs=probs1)
curve(dbeta(x, prm$shape1, prm$shape2), 0, 1, lwd=2, xlab="Vaccine effectiveness", ylab='Probability density')
abline(v=probs1, col='blue', lty=2)
mtext(c('min', '25th', 'median', '75th', 'max'), 3, at=probs1, line=0.5, col='blue', cex=0.75)
mtext('<12mos', 3, at=0.05, line=-1.5)

prm <- get.beta.params(quantiles=quantiles, probs=probs2)
curve(dbeta(x, prm$shape1, prm$shape2), 0, 1, lwd=2, xlab="Vaccine effectiveness", ylab='Probability density')
abline(v=probs2, col='blue', lty=2)
mtext('>12mos', 3, at=0.05, line=-1.5)


### Maximum Likelihood Estimation
get.beta.params(probs=rbeta(100, 1.5, 2))

# Measles vaccination coverage in Namibia
# WHO-UNICEF estiamtes from: http://apps.who.int/immunization_monitoring/globalsummary/estimates?c=NAM

mcv1 <- c(82, 80, 75, 85, 83, 82, 76, 74, 75, 76, 73, 69, 63, 73, 70, 70, 68, 58, 69, 65, 64, 59, 68)/100  	
mcv2 <- c(50, 32)/100

prm <- get.beta.params(probs=mcv1)
curve(dbeta(x, prm$shape1, prm$shape2), 0, 1)
abline(v=mcv1, lty=2, col='blue')

prm <- get.beta.params(probs=mcv2)
curve(dbeta(x, prm$shape1, prm$shape2), 0, 1)
abline(v=mcv2, lty=2, col='blue')
