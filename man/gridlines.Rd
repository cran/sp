\name{gridlines}
\alias{gridlines}

\title{ Create N-S and E-W grid lines over a geographic region }
\description{ Create N-S and E-W grid lines over a geographic region }
\usage{
gridlines(x, easts = pretty(bbox(x)[1,]), norths = pretty(bbox(x)[2,]), ndiscr = 20) 
}
\arguments{
  \item{x}{object deriving from class \link{Spatial-class}}
  \item{easts}{numeric; east-west values for vertical lines}
  \item{norths}{numeric; north-south values for horizontal lines}
  \item{ndiscr}{integer; number of points used to discretize the line, could
  be set to 2, unless the grid is (re)projected}
}

\value{ object of class \link{SpatialLines-class}, with lines as specified;
the return object inherits the projection information of \code{x}}

\details{  }

\author{
  Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl}, using example code
  of Roger Bivand.
}

\seealso{ Function transform in package \code{spproj} }

\examples{
data(meuse)
coordinates(meuse) = ~x+y
plot(meuse)
plot(gridlines(meuse), add=TRUE)
title("default gridlines within Meuse bounding box")
}

\keyword{spatial}
