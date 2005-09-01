\name{spsample-methods}
\docType{methods}
\alias{spsample-methods}
\alias{spsample,Spatial-method}
\alias{spsample,Line-method}
\alias{spsample,Polygon-method}
\alias{spsample,Polygons-method}
\alias{spsample,SpatialGrid-method}
\alias{spsample,SpatialPixels-method}
\alias{spsample,SpatialPolygons-method}
\title{ sampling point locations in (or on) a spatial object }
\description{
sampling point locations in (or on) a spatial object
}
\section{Methods}{
\describe{

\item{x = "Spatial"}{ sample in the bbox of \code{x} }

\item{x = "Line"}{ sample on a line }

\item{x = "Polygon"}{ sample in an Polygon }

\item{x = "Polygons"}{ sample in an Polygons object, consisting of possibly
multiple Polygon objects (and holes!) }

\item{x = "SpatialPolygons"}{ sample in an SpatialPolygons object; sampling
takes place over all Polygons objects present, use subsetting to vary
sampling intensity (density) }

\item{x = "SpatialGrid"}{ sample in an SpatialGrid object }

\item{x = "SpatialPixels"}{ sample in an SpatialPixels object }
}}
\seealso{\link{spsample}}
\keyword{methods}
