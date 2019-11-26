# Two dose vaccine
calc.prop.vacc(0.9, 0.9)
calc.prop.vacc(1, 0.5)
calc.prop.vacc(0.5, 0.5)

# Three dose vaccine
calc.prop.vacc(0.9, 0.9, 0.8)
calc.prop.vacc(1, 0.5, 0)
calc.prop.vacc(1, 0, 0.5)

# Should be equivalent
all.equal(calc.prop.vacc(0.999, 0.5)[,2], 
          calc.prop.vacc(0.999, 0.5, 0)[2:4,2], 
          tolerance=0.02)

all.equal(calc.prop.vacc(0.9, 0.8)[,2], 
          calc.prop.vacc(0.9, 0.8, 0)[2:4,2], 
          tolerance=0.02)

# Boundary conditions
calc.prop.vacc(0, 0)
calc.prop.vacc(1, 0)
calc.prop.vacc(1, 0, 0)
calc.prop.vacc(1, 1)
calc.prop.vacc(1, 1, 0)
calc.prop.vacc(1, 1, 1)
calc.prop.vacc(1, 0, 1)
calc.prop.vacc(0, 1, 1)

# The assumptions may not hold well when v1 << v2 << v3. 
# In this case its better to assume independence?
calc.prop.vacc(0.1, 0.9)
calc.prop.vacc(0.1, 0.5, 0.9)

# Calculate total proportion vaccinated with MCV1 and MCV2 with efficacy
p <- calc.prop.vacc(0.9, 0.8) 
sum(0.93*p[2,2], 0.97*p[1,2]) 

# Estimate posterior distribution of proportion vaccinated given uncertainty around MCV1 and MCV2
sims <- foreach(i=1:1000, .combine='c') %do% {
     
     p <- calc.prop.vacc(rbeta(1,40,1), rbeta(1,4,2))
     0.93*p[2,2] + 0.97*p[1,2] 
}

q <- quantile(sims, c(0.025, 0.5, 0.975))

par(mfrow=c(1,1))
hist(sims, breaks=100, col='cyan', xlab='Proportion vaccinated')
abline(v=q[2], lwd=3)
abline(v=q[c(1,3)], lty=2, lwd=2)
