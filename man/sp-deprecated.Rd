\name{sp-deprecated}
\alias{overlay}
\alias{overlay-methods}
\alias{overlay,SpatialPointsDataFrame,SpatialPolygons-method}
\alias{overlay,SpatialPoints,SpatialPolygons-method}
\alias{overlay,SpatialPoints,SpatialPolygonsDataFrame-method}
\alias{overlay,SpatialPolygons,SpatialPoints-method}
\alias{overlay,SpatialGridDataFrame,SpatialPolygons-method}
\alias{overlay,SpatialGrid,SpatialPolygons-method}
\alias{overlay,SpatialGrid,SpatialPolygonsDataFrame-method}
\alias{overlay,SpatialPolygons,SpatialGrid-method}
\alias{overlay,SpatialGridDataFrame,SpatialPoints-method}
\alias{overlay,SpatialGrid,SpatialPoints-method}
\alias{overlay,SpatialPixelsDataFrame,SpatialPoints-method}
\alias{overlay,SpatialPixels,SpatialPoints-method}
\alias{getSpPPolygonsIDSlots}
\alias{getSpPPolygonsLabptSlots}
\alias{getSpPnParts}
\alias{getSpPnHoles}
\alias{getSpPplotOrderSlot}
\alias{getSpPpolygonsSlot}
\alias{getPolygonAreaSlot}
\alias{getPolygonCoordsSlot}
\alias{getPolygonLabptSlot}
\alias{getPolygonHoleSlot}
\alias{getPolygonsIDSlot}
\alias{getPolygonsLabptSlot}
\alias{getPolygonsPolygonsSlot}
\alias{getPolygonsplotOrderSlot}
\alias{getLinesLinesSlot}
\alias{getLinesIDSlot}
\alias{getSLlinesSlot}
\alias{getSLLinesIDSlots}

\title{ Deprecated functions in sp }
\description{
Deprecated functions is sp: overlay, getSpP*, getPolygon*, getLines* getSL*
}
\usage{
overlay(x, y, ...)
}
\arguments{
\item{x}{first layer}
\item{y}{second layer, put on top of x}
\item{...}{ optional arguments; see example below }
}
\value{ 
a numerical array of indices of \code{x} on locations of \code{y}, or
a data.frame with (possibly aggregate) properties of \code{x} in units
of \code{y}. (Note that overlay is highly inconsistent, hence its deprecation)
}
\note{ For \code{overlay} the new implementation is found in the \link{over} method,
and works somewhat different (because consistent). }
\keyword{manip}
