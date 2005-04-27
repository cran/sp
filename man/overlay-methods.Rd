\name{overlay-methods}
\docType{methods}
\alias{overlay-methods}
\alias{overlay,SpatialPointsDataFrame,SpatialRings-method}
\alias{overlay,SpatialPoints,SpatialRings-method}
\alias{overlay,SpatialRings,SpatialPoints-method}
\alias{overlay,SpatialGridDataFrame,SpatialPoints-method}
\alias{overlay,SpatialGrid,SpatialPoints-method}
\title{ Methods for spatially overlay-ing points (grids) and rings layers }
\description{
overlay combines points (or grids) and rings by performing point-in-polygon
operation on all point-ring combinations.
}
\section{Methods}{
\describe{
\item{x = "SpatialPoints", y = "SpatialRings"}{ returns a numeric vector of length
equal to the number of points; the number is the id (number) of the ring of \code{y}
in which a point falls; NA denotes the point does not fall in a ring; if a point
falls in multiple rings, the last ring is recorded. 
}
\item{x = "SpatialPointsDataFrame", y = "SpatialRings"}{ equal to the previous method,
except that an argument \code{fn=xxx} is allowed, e.g. \code{fn = mean} which will 
then report a data.frame with the mean values of the \code{x} points fallin in each
ring (set) of \code{y} }
\item{x = "SpatialRings", y = "SpatialPoints"}{ returns the ring id of points in \code{y};
if \code{x} is a \code{SpatialRingDataFrame}, a data.frame with rows from \code{x}
corresponding to ponits in \code{y} is returned.
\item{x = "SpatialGridDataFrame", y = "SpatialPoints"}{ returns object of class
SpatialPointsDataFrame with grid attribute values x at spatial
point locations y; NA for NA grid cells or points outside grid, and NA values
on NA grid cells. }
\item{x = "SpatialGrid", y = "SpatialPoints"}{ returns grid values x at spatial
point locations y; NA for NA grid cells or points outside the grid }
}
}}
\author{Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl}}
\note{points on a ring boundary and points corresponding to a ring vertex are
considered to be inside the ring}
\seealso{\link{overlay}, \link{point.in.polygon}}
\keyword{methods}
