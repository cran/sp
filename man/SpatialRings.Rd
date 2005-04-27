\name{SpatialRings}
\alias{SpatialRings}
\alias{SpatialRingsDataFrame}
\alias{Sring}
\alias{Srings}
\title{ create objects of class SpatialRings or SpatialRingsDataFrame}
\description{ create objects of class \code{SpatialRings} or \code{SpatialRingsDataFrame} from
lists of \code{Srings} objects and \code{data.frame}s}
\usage{ 
Sring(coords, proj4string=CRS(as.character(NA)), hole=as.logical(NA))
Srings(srl, ID)
SpatialRings(Srl, pO = 1:length(Srl))
SpatialRingsDataFrame(Sr, data, match.ID = TRUE)

}
\arguments{
\item{coords}{2-column numeric matrix with coordinates; first point (row) should
equal last coordinates (row) }
\item{proj4string}{ projection string of class \link{CRS-class}}
\item{hole}{logical value for setting ring as hole or not}
\item{srl}{list with \link{Sring-class} objects}
\item{ID}{character vector of length one with identifier}
\item{Srl}{ list with objects of class \link{Srings-class}}
\item{pO}{ integer vector; plotting order }
\item{Sr}{ object of class \link{SpatialRings-class}}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data} should equal the number of \link{Srings-class} objects in \code{Sr}}
\item{match.ID}{logical: (default TRUE): match SpatialRings member Srings ID slot values with data frame row names, and re-order the data frame rows if necessary}
} 
\value{
\code{Sring} returns an object of class \code{Sring};
\code{Srings} returns an object of class \code{Srings};
\code{SpatialRings} returns object of class \code{SpatialRings};
\code{SpatialRingsDataFrame} returns object of class \code{SpatialRingsDataFrame}
}
\seealso{ \link{SpatialRings-class}, \link{SpatialRingsDataFrame-class} }

\keyword{manip}
