\name{spsample-methods}
\docType{methods}
\alias{spsample-methods}
\alias{spsample,Spatial-method}
\alias{spsample,Sline-method}
\alias{spsample,Sring-method}
\alias{spsample,Srings-method}
\alias{spsample,SpatialGrid-method}
\alias{spsample,SpatialPixels-method}
\alias{spsample,SpatialRings-method}
\title{ sampling point locations in (or on) a spatial object }
\description{
sampling point locations in (or on) a spatial object
}
\section{Methods}{
\describe{

\item{x = "Spatial"}{ sample in the bbox of \code{x} }

\item{x = "Sline"}{ sample on a line }

\item{x = "Sring"}{ sample in an Sring }

\item{x = "Srings"}{ sample in an Srings object, consisting of possibly
multiple Sring objects (and holes!) }

\item{x = "SpatialRings"}{ sample in an SpatialRings object; sampling
takes place over all Srings objects present, use subsetting to vary
sampling intensity (density) }

\item{x = "SpatialGrid"}{ sample in an SpatialGrid object }

\item{x = "SpatialPixels"}{ sample in an SpatialPixels object }
}}
\seealso{\link{spsample}}
\keyword{methods}
