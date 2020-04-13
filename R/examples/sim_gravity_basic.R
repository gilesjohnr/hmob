n <- 10
ids <- LETTERS[1:n]

# Distance matrix
D <- get.distance.matrix(x=rnorm(n, 100, 5), 
                         y=rnorm(n, 20, 2),
                         id=ids)

# Vector of population sizes
N <- rpois(n, 1000)
names(N) <- ids

# Simulate null model connectivity matrix
pi.hat <- sim.gravity(N=N, D=D)

# Simulate connectivity matrix given fitted gravity model parameters
pi.hat <- sim.gravity(N=N,
                      D=D,
                      theta=14,
                      omega.1=13,
                      omega.2=0.7,
                      gamma=1.5)

# Simulate trip counts based on fitted model parameters
M.hat <- sim.gravity(N=N,
                     D=D,
                     theta=14,
                     omega.1=13,
                     omega.2=0.7,
                     gamma=1.5,
                     counts=TRUE)