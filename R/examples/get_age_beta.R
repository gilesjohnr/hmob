# DHS vaccination coverage file downloaded from the website 
dhs <- read.csv('./data/Namibia2013.csv', stringsAsFactors=FALSE) 

# For all observations together (population mean)
tmp <- get.age.beta(age=dhs$age.in.months,
                    vacc=dhs$measles.y,
                    breaks=NULL)


# Two age groups < 12 months and > 12 months
tmp <- get.age.beta(age=dhs$age.in.months,
                    vacc=dhs$measles.y,
                    breaks=12)

par(mfrow=c(1,2))
for(i in 1:2) curve(dbeta(x, tmp$shape1[i], tmp$shape2[i]), 0, 1)


# 6-month age groups
tmp <- get.age.beta(age=dhs$age.in.months,
                    vacc=dhs$measles.y,
                    breaks=seq(0, 60, 6))

par(mfrow=c(2,5))
for(i in 1:10) curve(dbeta(x, tmp$shape1[i], tmp$shape2[i]), 0, 1)


# Each unique age in months
tmp <- get.age.beta(age=dhs$age.in.months,
                    vacc=dhs$measles.y,
                    breaks=seq(0, 60, 1))

par(mfrow=c(1,1))
plot(tmp$mu, type='l')


# 3-month age groups for each region
tmp <- foreach(i=unique(dhs$region.residence), .combine='rbind') %do% {
     
     sel <- dhs$region.residence == i
     
     cbind(
          data.frame(region=i),
          get.age.beta(age=dhs$age.in.months[sel],
                       vacc=dhs$measles.y[sel],
                       breaks=seq(0, 60, 3))
     )
}