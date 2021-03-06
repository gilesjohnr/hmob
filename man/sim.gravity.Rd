% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmob_funcs.R
\name{sim.gravity}
\alias{sim.gravity}
\title{Simulate connectivity values using gravity model}
\usage{
sim.gravity(
  N,
  D,
  theta = 1,
  omega.1 = 1,
  omega.2 = 1,
  gamma = 1,
  counts = FALSE
)
}
\arguments{
\item{N}{vector of population sizes}

\item{D}{matrix of distances among all \eqn{ij} pairs}

\item{theta}{scalar giving the proportionality constant of gravity formula (default = 1)}

\item{omega.1}{scalar giving exponential scaling of origin population size (default = 1)}

\item{omega.2}{scalar giving exponential scaling of destination population size (default = 1)}

\item{gamma}{scalar giving the dispersal kernel paramater (default = 1)}

\item{counts}{logical indicating whether or not to return a count variable by scaling the connectivity matrix by origin population size (\eqn{N_i}) (default = FALSE)}
}
\value{
a matrix with values between 0 and 1 (if \code{counts = FALSE}) or positive integers (if \code{counts = TRUE})
}
\description{
This function uses the gravity model formula to simulate a connectivity matrix based on the supplied model parameters. The 
gravity model formula uses a Gamma distribution as the dispersal kernel in the denominator. A null model (where all model parameters = 1) can be
simulated by supplying only population sizes (\code{N}) and pairwise distances (\code{D}).
\deqn{\theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )}
}
\examples{
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
}
\seealso{
Other simulation: 
\code{\link{calc.hpd}()},
\code{\link{calc.prop.inf}()},
\code{\link{calc.prop.remain}()},
\code{\link{calc.timing.magnitude}()},
\code{\link{calc.wait.time}()},
\code{\link{decay.func}()},
\code{\link{get.age.beta}()},
\code{\link{get.beta.params}()},
\code{\link{sim.TSIR.full}()},
\code{\link{sim.TSIR}()},
\code{\link{sim.combine.dual}()},
\code{\link{sim.combine}()},
\code{\link{sim.gravity.duration}()},
\code{\link{sim.lambda}()},
\code{\link{sim.pi}()},
\code{\link{sim.rho}()},
\code{\link{sim.tau}()}

Other gravity: 
\code{\link{fit.gravity}()},
\code{\link{fit.prob.travel}()},
\code{\link{sim.gravity.duration}()}
}
\author{
John Giles
}
\concept{gravity}
\concept{simulation}
