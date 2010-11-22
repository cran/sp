\name{over-methods}
\docType{methods}
\alias{over}
\alias{\%over\%}
\alias{over-methods}
\alias{over,SpatialPoints,SpatialPoints-method}
\alias{over,SpatialPoints,SpatialPolygons-method}
\alias{over,SpatialPoints,SpatialPolygonsDataFrame-method}
\alias{over,SpatialGridDataFrame,SpatialPolygonsDataFrame-method}
\alias{over,SpatialPolygons,SpatialPoints-method}
\alias{over,SpatialPolygons,SpatialPointsDataFrame-method}
\alias{over,SpatialGridDataFrame,SpatialPoints-method}
\alias{over,SpatialGrid,SpatialPoints-method}
\alias{over,SpatialPixelsDataFrame,SpatialPoints-method}
\alias{over,SpatialPixels,SpatialPoints-method}
\alias{over,SpatialPoints,SpatialGrid-method}
\alias{over,SpatialPoints,SpatialGridDataFrame-method}
\alias{over,SpatialPoints,SpatialPixels-method}
\alias{over,SpatialPoints,SpatialPixelsDataFrame-method}
\alias{over,SpatialPolygons,SpatialGridDataFrame-method}
\title{ consistent spatial overlay for points, grids and polygons }
\description{ consistent spatial overlay for points, grids and polygons:
retrieves the indexes or attributes from one geometry at the spatial
locations of another }
\usage{
over(x, y, returnList = FALSE, fn = NULL)
x %over% y
}
\arguments{
\item{x}{geometry (locations) of the queries}
\item{y}{layer from which the geometries or attributes are queried}
\item{returnList}{ logical; see value }
\item{fn}{(optional) a function; see value }
}
\value{ 
an object of length \code{length(x)}, or a data.frame with number
of rows equal to \code{length(x)}. If \code{returnList} is FALSE,
a vector with indices of \code{y} for each geometry (point, grid
cell centre, polygon or lines) in \code{x}.  if \code{returnList} is
TRUE, a list of length \code{length(x)}, with list element \code{i}
the vector of indices of the geometries in \code{y} that correspond
to the $i$-th geometry in \code{x}.
}

\section{Methods}{
\describe{
\item{x = "SpatialPoints", y = "SpatialPolygons"}{ returns a numeric
vector of length equal to the number of points; the number is the
index (number) of the polygon of \code{y} in which a point falls;
NA denotes the point does not fall in a polygon; if a point falls
in multiple polygons, the last polygon is recorded.  }
\item{x = "SpatialPointsDataFrame", y = "SpatialPolygons"}{ equal
to the previous method, except that an argument \code{fn=xxx} is
allowed, e.g. \code{fn = mean} which will then report a data.frame
with the mean values of the \code{x} points falling in each polygon
(set) of \code{y} }
\item{x = "SpatialPoints", y = "SpatialPolygonsDataFrame"}{ returns
a data.frame of the second argument with row entries corresponding
to the first argument }
\item{x = "SpatialPolygons", y = "SpatialPoints"}{ returns
the polygon index of points in \code{y}; if \code{x} is
a \code{SpatialPolygonsDataFrame}, a data.frame with rows from
\code{x} corresponding to points in \code{y} is returned.}
\item{x = "SpatialGridDataFrame", y = "SpatialPoints"}{ returns
object of class SpatialPointsDataFrame with grid attribute values
x at spatial point locations y; NA for NA grid cells or points
outside grid, and NA values on NA grid cells. }
\item{x = "SpatialGrid", y = "SpatialPoints"}{ returns grid values
x at spatial point locations y; NA for NA grid cells or points
outside the grid }
\item{x = "SpatialPixelsDataFrame", y = "SpatialPoints"}{ returns
grid values x at spatial point locations y; NA for NA grid cells
or points outside the grid }
\item{x = "SpatialPixels", y = "SpatialPoints"}{ returns grid
values x at spatial point locations y; NA for NA grid cells or
points outside the grid }
\item{x = "SpatialPoints", y = "SpatialGrid"}{ xx }
\item{x = "SpatialPoints", y = "SpatialGridDataFrame"}{ xx }
\item{x = "SpatialPoints", y = "SpatialPixels"}{ xx }
\item{x = "SpatialPoints", y = "SpatialPixelsDataFrame"}{ xx }
\item{x = "SpatialPolygons", y = "SpatialGridDataFrame"}{ xx }
}
}
\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}

\note{points on a polygon boundary and points corresponding to a
polygon vertex are considered to be inside the polygon.

These methods assume that pixels and polygons are never overlapping;
if they are (and a point falls in more than one pixel/polygon),
only one of the geometries concerned is addressed.

}
\seealso{\link{overlay}, \link{point.in.polygon}}
\keyword{methods}
