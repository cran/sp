\name{GridTopology-class}
\docType{class}
\alias{GridTopology-class}
\alias{print.summary.GridTopology}
\alias{summary.GridTopology}
\alias{show,GridTopology-method}

\title{Class "GridTopology" ~~~ }
\description{ class for defining a rectangular grid of
arbitrary dimension }
\section{Objects from the Class}{
Objects are created by using e.g. 

GridTopology(c(0,0), c(1,1), c(5,5))

see \link{SpatialGrid}
}
\section{Slots}{
  \describe{
    \item{\code{cellcentre.offset}:}{numeric; vector with the smallest coordinates for
	each dimension; coordinates refer to the cell centre }
    \item{\code{cellsize}:}{numeric; vector with the cell size in each dimension}
    \item{\code{cells.dim}:}{integer; vector with number of cells in each dimension}
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection  }
  }
}

\section{Extends}{
% Class \code{"Spatial"}, by class \code{"SpatialPoints"}.
}
\section{Methods}{
 \describe{
   \item{coordinates}{\code{signature(x = "SpatialGrid")}: calculates coordinates for
   each point on the grid
   }
 }
}
\references{ }
\author{ Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl} }
\note{  }

\seealso{
  \code{\link{SpatialGridDataFrame-class}}, \code{\link{SpatialGrid-class}}
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
