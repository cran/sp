\name{as.SpatialPolygons.GridTopology}
\alias{as.SpatialPolygons.GridTopology}
\alias{IDvaluesGridTopology}
\alias{as.SpatialPolygons.SpatialPixels}
\alias{IDvaluesSpatialPixels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make SpatialPolygons object from GridTopology object}
\description{
  Converts grids of regular rectangles into a \code{SpatialPolygons} object, which can be transformed to a different projection or datum. The function is not suitable for high-resolution grids. The ordering of the grid cells is as in \code{coordinates()} of the same object, and is reported by \code{IDvaluesGridTopology}.
}
\usage{
as.SpatialPolygons.GridTopology(grd, proj4string = CRS(as.character(NA)))
IDvaluesGridTopology(obj)
as.SpatialPolygons.SpatialPixels(obj, proj4string=CRS(as.character(NA)))
IDvaluesSpatialPixels(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd,obj}{\code{GridTopology} object or \code{SpatialPixels} object}
  \item{proj4string}{projection string of class CRS}
}

\value{
  \code{as.SpatialPolygons.GridTopology} and \code{as.SpatialPolygons.SpatialPixels} return a \code{SpatialPolygons} object;
\code{IDvaluesGridTopology} and \code{IDvaluesSpatialPixels} return a character vector with the object grid indices.
}

\seealso{\code{\link{GridTopology}}, \code{\link{SpatialPixels}}, \code{\link{SpatialPolygons}}}

\examples{
grd <- GridTopology(cellcentre.offset=c(-175,55), cellsize=c(10,10), cells.dim=c(4,4))
SpP_grd <- as.SpatialPolygons.GridTopology(grd)
plot(SpP_grd)
text(getSpPPolygonsLabptSlots(SpP_grd), getSpPPolygonsIDSlots(SpP_grd), cex=0.5)
trdata <- data.frame(A=rep(c(1,2,3,4), 4), B=rep(c(1,2,3,4), each=4), row.names=getSpPPolygonsIDSlots(SpP_grd))
SpPDF <- SpatialPolygonsDataFrame(SpP_grd, trdata)
spplot(SpPDF)
}
\keyword{spatial}
