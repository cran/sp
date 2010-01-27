\name{overlay-methods}
\docType{methods}
\alias{overlay-methods}
\alias{overlay,SpatialPointsDataFrame,SpatialPolygons-method}
\alias{overlay,SpatialPoints,SpatialPolygons-method}
\alias{overlay,SpatialPolygons,SpatialPoints-method}
\alias{overlay,SpatialGridDataFrame,SpatialPoints-method}
\alias{overlay,SpatialGrid,SpatialPoints-method}
\alias{overlay,SpatialPixelsDataFrame,SpatialPoints-method}
\alias{overlay,SpatialPixels,SpatialPoints-method}
\title{ Methods for spatially overlay-ing points (grids) and polygons layers }
\description{
overlay combines points (or grids) and polygons by performing point-in-polygon
operation on all point-polygons combinations.
}
\section{Methods}{
\describe{
\item{x = "SpatialPoints", y = "SpatialPolygons"}{ returns a numeric vector of length
equal to the number of points; the number is the index (number) of the polygon of \code{y}
in which a point falls; NA denotes the point does not fall in a polygon; if a point
falls in multiple polygons, the last polygon is recorded.  }
\item{x = "SpatialPointsDataFrame", y = "SpatialPolygons"}{ equal to the previous method,
except that an argument \code{fn=xxx} is allowed, e.g. \code{fn = mean} which will 
then report a data.frame with the mean values of the \code{x} points falling in each
polygon (set) of \code{y} }
\item{x = "SpatialPolygons", y = "SpatialPoints"}{ returns the polygon index of points in \code{y};
if \code{x} is a \code{SpatialPolygonsDataFrame}, a data.frame with rows from \code{x}
corresponding to points in \code{y} is returned.}
\item{x = "SpatialGridDataFrame", y = "SpatialPoints"}{ returns object of class
SpatialPointsDataFrame with grid attribute values x at spatial
point locations y; NA for NA grid cells or points outside grid, and NA values
on NA grid cells. }
\item{x = "SpatialGrid", y = "SpatialPoints"}{ returns grid values x at spatial
point locations y; NA for NA grid cells or points outside the grid }
\item{x = "SpatialPixelsDataFrame", y = "SpatialPoints"}{ returns grid values x at 
spatial point locations y; NA for NA grid cells or points outside the grid }
\item{x = "SpatialPixels", y = "SpatialPoints"}{ returns grid values x at spatial
point locations y; NA for NA grid cells or points outside the grid }
}
}
\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}
\note{points on a polygon boundary and points corresponding to a polygon vertex are
considered to be inside the polygon}
\seealso{\link{overlay}, \link{point.in.polygon}}
\keyword{methods}
