\name{SpatialPolygons}
\alias{SpatialPolygons}
\alias{SpatialPolygonsDataFrame}
\alias{Polygon}
\alias{Polygons}
\title{ create objects of class SpatialPolygons or SpatialPolygonsDataFrame}
\description{ create objects of class \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} from
lists of \code{Polygons} objects and \code{data.frame}s}
\usage{ 
Polygon(coords, hole=as.logical(NA))
Polygons(srl, ID)
SpatialPolygons(Srl, pO, proj4string=CRS(as.character(NA)))
SpatialPolygonsDataFrame(Sr, data, match.ID = TRUE)

}
\arguments{
\item{coords}{2-column numeric matrix with coordinates; first point (row) should
equal last coordinates (row) }
\item{proj4string}{ projection string of class \link{CRS-class}}
\item{hole}{logical value for setting polygon as hole or not}
\item{srl}{list with \link{Polygon-class} objects}
\item{ID}{character vector of length one with identifier}
\item{Srl}{ list with objects of class \link{Polygons-class}}
\item{pO}{ integer vector; plotting order; if missing in reverse order of Polygons area }
\item{Sr}{ object of class \link{SpatialPolygons-class}}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data} should equal the number of \link{Polygons-class} objects in \code{Sr}}
\item{match.ID}{logical: (default TRUE): match SpatialPolygons member Polygons ID slot values with data frame row names, and re-order the data frame rows if necessary}
} 
\value{
\code{Polygon} returns an object of class \code{Polygon};
\code{Polygons} returns an object of class \code{Polygons};
\code{SpatialPolygons} returns object of class \code{SpatialPolygons};
\code{SpatialPolygonsDataFrame} returns object of class \code{SpatialPolygonsDataFrame}
}
\seealso{ \link{SpatialPolygons-class}, \link{SpatialPolygonsDataFrame-class} }

\keyword{manip}
