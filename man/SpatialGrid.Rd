\name{SpatialPixels}
\alias{SpatialPixels}
\alias{SpatialGrid}
\alias{GridTopology}
\alias{SpatialGrid}
\alias{plot.SpatialGrid}
\alias{coordinatevalues}
\alias{points2grid}
\alias{getGridIndex}
\alias{getGridTopology}
\alias{areaSpatialGrid}
\title{ define spatial grid }
\description{
defines spatial grid by offset, cell size and dimensions
}
\usage{
GridTopology(cellcentre.offset, cellsize, cells.dim)
SpatialPixels(points, tolerance = sqrt(.Machine$double.eps))
SpatialGrid(grid, proj4string = CRS(as.character(NA)))
coordinatevalues(obj)
points2grid(points, tolerance = sqrt(.Machine$double.eps))
getGridIndex(cc, grid, all.inside = TRUE)
getGridTopology(obj)
areaSpatialGrid(obj)
}
\arguments{
    \item{cellcentre.offset}{numeric; vector with the smallest coordinates for
    each dimension}
    \item{cellsize}{numeric; vector with the cell size in each dimension}
    \item{cells.dim}{integer; vector with number of cells in each dimension}
	\item{points}{ coordinates, object of class \link{SpatialPoints-class} }
	\item{grid}{ grid topology; object of class \link{GridTopology-class}}
	\item{tolerance}{ precision, used to which extent points are exactly on
	a grid}
	\item{proj4string}{ object of class \link{CRS-class}}
	\item{obj}{object of class or deriving from \link{SpatialGrid-class}}
	\item{cc}{ numeric matrix with coordinates }
	\item{all.inside}{ logical; if TRUE and \code{cc} points fall outside
	the grid area, an error message is generated; if FALSE, NA values
	are generated for such points }
}

\value{ GridTopology returns a value of class \link{GridTopology-class};
SpatialGrid returns an object of class \link{SpatialGrid-class}

\code{coordinatevalues} returns a list with the unique x-coordinates,
the unique y-coordinate, etc. instead of the \link{coordinates} of all
grid cells

SpatialGrid returns an object of class \link{SpatialGrid-class}. 

points2grid returns the \link{GridTopology-class} from a set of points.

getGridIndex finds the index of a set of point coordinates in a given
grid topology, and depending on \code{all.inside} setting, generates NA
or an error message if points are outside the grid domain.

getGridTopology returns the slot of class \link{GridTopology-class} from
obj.

areaSpatialGrid returns the spatial area of (the non-missing valued cells
of) the grid. For objects of class \link{SpatialGridDataFrame-class}
the area refers to cells where any (one or more) of the attribute columns
are non-missing valued.
}

\note{ 
SpatialGrid stores grid topology and may or may not store the coordinates
of the actual points, which may form a subset of the full grid. To find
out or change this, see \link{fullgrid}.

points2grid tries to figure out the grid topology from points. It succees
only if points on a grid line have constant y column, and points on a
grid column have constant x coordinate, etc. In other cases, use signif
on the raw coordinate matrices to make sure this is the case.
}

\references{ }

\author{ Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl}}

\seealso{ 
\link{SpatialGrid-class},
\link{SpatialGridDataFrame-class},
}

\examples{
x = GridTopology(c(0,0), c(1,1), c(5,4))
class(x)
x
summary(x)
coordinates(x)
coordinates(GridTopology(c(0,0), c(1,1), c(5,4)))
coordinatevalues(x)
}

\keyword{spatial}
