% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmob_model_funcs.R
\name{fit.gravity}
\alias{fit.gravity}
\title{Fit gravity model to movement matrix}
\usage{
fit.gravity(
  M,
  D,
  N = NULL,
  N.orig = NULL,
  N.dest = NULL,
  n.chain = 2,
  n.burn = 1000,
  n.samp = 1000,
  n.thin = 1,
  prior = NULL,
  DIC = FALSE,
  parallel = FALSE
)
}
\arguments{
\item{M}{named matrix of trip counts among all \eqn{ij} location pairs}

\item{D}{named matrix of distances among all \eqn{ij} location pairs}

\item{N}{named vector of population sizes for all locations (either N or both N.orig and N.dest must be supplied)}

\item{N.orig}{named vector of population sizes for each origin}

\item{N.dest}{named vector of population sizes for each destination.}

\item{n.chain}{number of MCMC sampling chains}

\item{n.burn}{number of iterations to discard before sampling of chains begins (burn in)}

\item{n.samp}{number of iterations to sample each chain}

\item{n.thin}{interval to thin samples}

\item{DIC}{logical indicating whether or not to calculate the Deviance Information Criterion (DIC) (default = FALSE)}

\item{parallel}{logical indicating whether or not to run MCMC chains in parallel or sequentially (default = FALSE)}
}
\value{
a runjags model object containing fitted gravity model paramters
}
\description{
This function fits gravity model parameters to a supplied movement matrix using Bayesian MCMC inference. The function defines the model and serves as a wrapper for the \code{\link{run.jags}}
function in the \code{\link{runjags}} package. Gravity model formula:
\deqn{
    \theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )
}
}
\examples{
M <- load.obj(2, './data/movement_data_arrays_full.Rdata')            # M.route
D <- load.obj(1, './data/distance_matrix_named.rdata')                # distance.matrix
N <- load.obj(1, './data/N_pop.rdata')                                # N.pop

M <- M[1:5, 1:5]

districts <- attributes(M)$dimnames$origin 
D <- D[districts, districts]
N <- N[names(N) \%in\% districts]

out <- fit.gravity(M=M,
                   D=D,
                   N=N,
                   n.chain=2,
                   n.adapt=1000,
                   n.burn=4000,
                   n.samp=4000,
                   n.thin=1,
                   parallel=F)

summary(out)

}
\seealso{
Other model: 
\code{\link{fit.jags}()},
\code{\link{fit.prob.travel}()},
\code{\link{rjags.combine}()},
\code{\link{rjags.mcmc.list}()},
\code{\link{summarize.hmob}()}

Other gravity: 
\code{\link{fit.prob.travel}()},
\code{\link{sim.gravity.duration}()},
\code{\link{sim.gravity}()}
}
\author{
John Giles
}
\concept{gravity}
\concept{model}
