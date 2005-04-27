\name{SpatialRings-class}
\docType{class}
\alias{SpatialRings-class}
\alias{[,SpatialRings-method}
\alias{as.data.frame.SpatialRings}
\alias{plotSpatialRings}
\alias{plot.SpatialRings}

\title{Class "SpatialRings"}
\description{  class to hold polygon topology (without attributes) }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{SpatialRings}
}
\section{Slots}{
  \describe{
    \item{\code{polygons}:}{Object of class \code{"list"}; list elements are
	all of class \link{Srings-class}}
    \item{\code{plotOrder}:}{Object of class \code{"integer"}; integer array
	giving the order in which objects should be plotted }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
No methods defined with class "SpatialRings" in the signature.
}
\references{ }
\author{ Roger Bivand }
\note{ }
\seealso{ \link{SpatialRings} }
\examples{
data(ncshp)
nc1 <- as.SpatialRings.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
plotSpatialRings(nc1)
invisible(text(getSRSringsLabptSlots(nc1), labels=getSRSringsIDSlots(nc1), cex=0.6))
data(ncmap)
IDs <- sapply(strsplit(ncmap$names, ","), function(x) x[2])
nc2 <- as.SpatialRings.map(ncmap, IDs)
plotSpatialRings(nc2)
invisible(text(getSRSringsLabptSlots(nc2), labels=getSRSringsIDSlots(nc2), cex=0.6))
data(co37_d90_arc)
data(co37_d90_pal)
data(co37_d90_pat)
IDs <- paste(pat$ST[-1], pat$CO[-1], sep="")
nc3 <- as.SpatialRings.pal(arc, pal, IDs=IDs)
plotSpatialRings(nc3)
invisible(text(getSRSringsLabptSlots(nc3), labels=getSRSringsIDSlots(nc3), cex=0.6))
}
\keyword{classes}
