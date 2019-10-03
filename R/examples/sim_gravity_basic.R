B <- load.obj(2, './output/gravity_model_basic_route_full4.Rdata')

# Simulate connectivity matrix given fitted gravity model parameters
pi.hat <- sim.gravity(N=N,
                      D=D,
                      theta=B['theta', 'Mean'],
                      omega.1=B['omega.1', 'Mean'],
                      omega.2=B['omega.2', 'Mean'],
                      s=B['s', 'Mean'],
                      r=B['r', 'Mean'])

# Simulate null model connectivity matrix
pi.hat <- sim.gravity(N=N, D=D)

# Simulate trip counts based on fitted model parameters
M.hat <- sim.gravity(N=N,
                     D=D,
                     theta=B['theta', 'Mean'],
                     omega.1=B['omega.1', 'Mean'],
                     omega.2=B['omega.2', 'Mean'],
                     s=B['s', 'Mean'],
                     r=B['r', 'Mean'],
                     counts=TRUE)
