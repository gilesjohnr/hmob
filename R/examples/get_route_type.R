y.route <- load.obj(3, file='./data/duration_data_arrays_1day_full.Rdata') # y.month, y.route, y.pop
y.route <- get.subsamp(y.route, min.locations=30, min.samp=20)
districts <- dimnames(y.route)$origin
n.districts <- length(dimnames(y.route)$origin)

lam1 <- load.obj(1, './output/decay_1day_62dists_summary.Rdata')
lam1 <- get.param.vals(n.districts, name='lambda', stats=lam1, type='dataframe')

# clean
lam1$from <- districts[lam1$from]
lam1$to <- districts[lam1$to]

# Route type criteria
pop <- read.csv("./data/population_data.csv")
cutoff <- 980
hi <- pop$ID_2[pop$density >= cutoff]
lo <- pop$ID_2[pop$density < cutoff]

lam1$type <- get.route.type(lam1, orig=2, dest=3, hi=hi, lo=lo)
