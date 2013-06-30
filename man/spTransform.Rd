\name{spTransform}
\alias{spTransform-methods}
\alias{spTransform,ANY,ANY-method}
\alias{spTransform,ANY,CRS-method}
\alias{spTransform}
\title{ spTransform for map projection and datum transformation }
\description{ spTransform for map projection and datum transformation }
\usage{
spTransform(x, CRSobj, ...)
}
\arguments{
\item{x}{ object to be transformed }
\item{CRSobj}{ object of class \link{CRS}}
\item{...}{further arguments}
}
\value{
object with coordinates transformed to the new coordinate reference
system.
}
\note{
Package \code{sp} only provides the definition of the generic, 
methods doing actual transformation are found in package \code{rgdal},
see \link[rgdal]{spTransform}
}
\keyword{methods}
\keyword{spatial}
