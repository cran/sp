\name{spDistsN1}
\alias{spDistsN1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Euclidean or Great Circle distance between points}
\description{
  The function returns a vector of distances between a matrix of 2D points and a single 2D point, using Euclidean or Great Circle distance (WGS84 ellipsoid) methods.
}
\usage{
spDistsN1(pts, pt, longlat = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pts}{A matrix of 2D points}
  \item{pt}{A single 2D point}
  \item{longlat}{if FALSE, Euclidean distance, if TRUE Great Circle distance}
}

\value{
  A numeric vector of distances in the metric of the points if longlat=FALSE, or in kilometers if longlat=TRUE
}
\note{The function can also be used to find a local kilometer equivalent to a plot scaled in decimal degrees in order to draw a scale bar.}
\references{\url{http://home.att.net/~srschmitt/script_greatcircle.html}}
\author{Roger Bivand}


\seealso{\code{\link{is.projected}}}
\examples{
ll <- matrix(c(5, 6, 60, 60), ncol=2)
km <- spDistsN1(ll, ll[1,], longlat=TRUE)
zapsmall(km)
utm32 <- matrix(c(276.9799, 332.7052, 6658.1572, 6655.2055), ncol=2)
spDistsN1(utm32, utm32[1,])
dg <- spDistsN1(ll, ll[1,])
dg
dg[2]/km[2]
}
\keyword{spatial}
