\name{gridlines}
\alias{gridlines}
\alias{gridat}

\title{ Create N-S and E-W grid lines over a geographic region }
\description{ Create N-S and E-W grid lines over a geographic region; \code{gridat} permits the construction of points and labels for non-projected grid annotation }
\usage{
gridlines(x, easts = pretty(bbox(x)[1,]), norths = pretty(bbox(x)[2,]), ndiscr = 20) 
gridat(x, easts = pretty(bbox(x)[1,]), norths = pretty(bbox(x)[2,]), offset=0.5)
}
\arguments{
  \item{x}{object deriving from class \link{Spatial-class}}
  \item{easts}{numeric; east-west values for vertical lines}
  \item{norths}{numeric; north-south values for horizontal lines}
  \item{ndiscr}{integer; number of points used to discretize the line, could
  be set to 2, unless the grid is (re)projected}
  \item{offset}{offset value to be returned, see \link{text}}
}

\value{ \code{gridlines} returns an object of class \link{SpatialLines-class}, with lines as specified;
the return object inherits the projection information of \code{x}; \code{gridat} returns a SpatialPointsDataFrame with points at the west and south ends of the grid lines created by \code{gridlines}, with degree labels}

\author{
  Edzer J. Pebesma, \email{edzer.pebesma@uni-muenster.de}, using example code
  of Roger Bivand.
}

\seealso{ Function spTransform in package \code{rgdal} }

\examples{
data(meuse)
coordinates(meuse) = ~x+y
plot(meuse)
plot(gridlines(meuse), add=TRUE)
title("default gridlines within Meuse bounding box")
}

\keyword{spatial}
