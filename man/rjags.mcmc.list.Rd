% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmob_model_funcs.R
\name{rjags.mcmc.list}
\alias{rjags.mcmc.list}
\title{Convert rjags object to mcmc.list}
\usage{
rjags.mcmc.list(x)
}
\arguments{
\item{x}{an rjags list}
}
\value{
an \code{\link[coda:mcmc.list]{mcmc.list}} object
}
\description{
This function converts an rjags object to an mcmc.list.
}
\seealso{
Other model: 
\code{\link{fit.gravity}()},
\code{\link{fit.jags}()},
\code{\link{fit.prob.travel}()},
\code{\link{rjags.combine}()},
\code{\link{summarize.hmob}()}

Other utility: 
\code{\link{check.distance.matrix}()},
\code{\link{get.admin.level}()},
\code{\link{get.legend}()},
\code{\link{get.names}()},
\code{\link{get.pop.vec}()},
\code{\link{get.unique.coords}()},
\code{\link{get.unique.names}()},
\code{\link{load.obj}()},
\code{\link{rjags.combine}()},
\code{\link{see}()},
\code{\link{trip.map}()}
}
\author{
John Giles
}
\concept{model}
\concept{utility}
