% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmob_funcs.R
\name{get.names}
\alias{get.names}
\title{Get title for any origin and destination}
\usage{
get.names(a, b = NULL, d = districtIDs)
}
\arguments{
\item{a}{origin district (can take integer ID or character name)}

\item{b}{destination district (can take integer ID or character name), default = NULL}

\item{d}{district IDs and names, expacts the \code{districtIDs} data object (NamNames.csv)}
}
\value{
character string
}
\description{
Makes character strings given origin and destination and districtIDs. Use with \code{trip.map} function:
\enumerate{
\item The \code{trip.map} function takes the origin \code{a} and destination \code{b} and makes a simple map to visualize the districts being used. Origin is a blue triangle, destination is a red circle.
\item The \code{get.names} function prints a 'from and to' character string with district names.
}
}
\examples{
par(mfrow=c(1,2))
trip.map(28, 20) # Generate map with district names
title(get.names(28, 20))
trip.map(42, c(16, 20, 1)) # Get map with multiple destinations
title(get.names(42))
}
\seealso{
Other utility: 
\code{\link{check.distance.matrix}()},
\code{\link{get.admin.level}()},
\code{\link{get.legend}()},
\code{\link{get.pop.vec}()},
\code{\link{get.unique.coords}()},
\code{\link{get.unique.names}()},
\code{\link{load.obj}()},
\code{\link{rjags.combine}()},
\code{\link{rjags.mcmc.list}()},
\code{\link{see}()},
\code{\link{trip.map}()}
}
\author{
John Giles
}
\concept{utility}
