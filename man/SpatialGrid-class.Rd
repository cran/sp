\name{SpatialGrid-class}
\docType{class}
\alias{SpatialGrid-class}
\alias{[,SpatialGrid-method}
\alias{print.summary.SpatialGrid}
\alias{as.data.frame.SpatialGrid}
\alias{summary,SpatialGrid-method}
\alias{coerce,SpatialGrid,data.frame-method}
\alias{show,SpatialGrid-method}

\title{Class "SpatialGrid" ~~~ }
\description{ class for defining a full, rectangular grid of
arbitrary dimension }
\section{Objects from the Class}{
Objects are created by using e.g. 

SpatialGrid(grid)

with grid of class \link{GridTopology-class}
}
\section{Slots}{
  \describe{
	\item{\code{grid}}{ object of class \link{GridTopology-class}, defining the
	grid topology (offset, cellsize, dim) }
	\item{\code{grid.index}}{ index of points in full grid, or integer(0) }
	\item{\code{coords}}{ coordinates of points, or bbox of grid }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection  }
  }
}
\section{Extends}{
Class \code{"SpatialPoints"} directly;
Class \code{"Spatial"}, by class \code{"SpatialPoints"}.
}
\section{Methods}{
 \describe{
   \item{coordinates}{\code{signature(x = "SpatialGrid")}: calculates coordinates for
   each point on the grid; coordinates are not stored in objects of class SpatialGrid}
   \item{summary}{\code{signature(object = "SpatialGrid")}: summarize object}
   \item{plot}{\code{signature(x = "SpatialGrid")}: plots cell centers }
   \item{"["}{\code{signature(x = "SpatialGrid")}: select rows and columns }
 }
}
\references{ }
\author{ Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl} }
\note{  }

\seealso{
  \code{\link{SpatialGridDataFrame-class}}, \code{\link{SpatialGrid}}
}
\examples{
x = GridTopology(c(0,0), c(1,1), c(5,5))
class(x)
x
summary(x)
coordinates(x)
y = SpatialGrid(grid = x)
class(y)
y
}
\keyword{classes}
