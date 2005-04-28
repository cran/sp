\name{meuse.grid}
\alias{meuse.grid}
\non_function{}
\title{ Prediction Grid for Meuse Data Set }
\usage{data(meuse.grid)}
\description{
The \code{meuse.grid} data frame has 3103 rows and 2 columns;
a grid with 40 m x 40 m spacing that covers the Meuse Study area
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{x}{a numeric vector; x-coordinate (see \link{meuse}) }
    \item{y}{a numeric vector; y-coordinate (see \link{meuse}) }
	\item{dist}{distance to the Meuse river; obtained by a spread
	(spatial distance) GIS operation, from border of river; normalized to
	$[0,1]$ }
	\item{ffreq}{flood frequency; the lower the value, the larger the flood frequency; the origin of this item is questionable}
	\item{part.a}{arbitrary division of the area in two areas, a and b}
	\item{part.b}{see \code{part.a}}
	\item{soil}{soil type; it is questionable whether these data come from a real soil
	map}
  }
}
\details{
\code{x} and \code{y} are in RDM, the Dutch topographical map
coordinate system. Roger Bivand projected this to UTM in the
R-Grass interface package.
}
\source{
\url{http://www.gstat.org/}
}
\references{
See the \link{meuse} documentation
}
\examples{
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE
spplot(meuse.grid)
}
\keyword{datasets}
