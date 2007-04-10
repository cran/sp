\name{SpatialPolygonsDataFrame-class}
\docType{class}
\alias{SpatialPolygonsDataFrame-class}
\alias{[,SpatialPolygonsDataFrame-method}
\alias{summary,SpatialPolygonsDataFrame-method}
\alias{$.SpatialPolygonsDataFrame}
\alias{$<-.SpatialPolygonsDataFrame}
\alias{as.data.frame.SpatialPolygonsDataFrame}
\alias{coerce,SpatialPolygonsDataFrame,data.frame-method}
\alias{coerce,SpatialPolygonsDataFrame,SpatialLinesDataFrame-method}
\alias{rbind.SpatialPolygonsDataFrame}

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

\title{Class "SpatialPolygonsDataFrame" }
\description{ class to hold polygons with attributes }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{SpatialPolygonsDataFrame}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}; attribute table }
    \item{\code{polygons}:}{Object of class \code{"list"}; see \link{SpatialPolygons-class} }
    \item{\code{plotOrder}:}{Object of class \code{"integer"}; see \link{SpatialPolygons-class} }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class}}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}
\section{Extends}{
Class \code{"SpatialPolygons"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialPolygons"}.
}
\section{Methods}{
Methods defined with class "SpatialPolygonsDataFrame" in the signature:
  \describe{
    \item{[}{\code{signature(x = "SpatialPolygonsDataFrame")}: select subset of (sets of) polygons; NAs are not permitted in the row index}
	\item{summary}{\code{\signature(object = "SpatialPolygonsDataFrame")}: summarize object}
  }
}
\references{ }
\author{ }
\note{\code{SpatialPolygonsDataFrame} with default ID matching checks the data frame row names against the Polygons ID slots. They must then agree with each other, and be unique (no Polygons objects can share IDs); the data frame rows will be re-ordered if needed to match the Polygons IDs.}

\seealso{
	\link{SpatialPolygons-class}
}
\examples{
%data(ncshp)
%nc1 <- as.SpatialPolygons.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
%df <- nc.shp$att.data
%rownames(df) <- as.character(nc.shp$att.data$FIPS)
%identical(rownames(df), getSpPPolygonsIDSlots(nc1))
%ncSpPDF <- SpatialPolygonsDataFrame(nc1, df)
%names(as(ncSpPDF, "data.frame"))
%identical(rownames(as(ncSpPDF, "data.frame")), getSpPPolygonsIDSlots(ncSpPDF))
%rrt <- as(ncSpPDF, "data.frame")$SID74/as(ncSpPDF, "data.frame")$BIR74
%brks <- quantile(rrt, seq(0,1,1/7))
%cols <- grey((length(brks):2)/length(brks))
%dens <- (2:length(brks))*3
%plot(ncSpPDF, col=cols[findInterval(rrt, brks, all.inside=TRUE)])
%plot(ncSpPDF, density=dens[findInterval(rrt, brks, all.inside=TRUE)])
grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as.SpatialPolygons.GridTopology(grd)
centroids <- getSpPPolygonsLabptSlots(polys)
x <- centroids[,1]
y <- centroids[,2]
z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
ex_1.7 <- SpatialPolygonsDataFrame(polys, data=data.frame(x=x, y=y, z=z, row.names=getSpPPolygonsIDSlots(polys)))
brks <- quantile(z, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
plot(ex_1.7, col=cols[findInterval(z, brks, all.inside=TRUE)])
plot(ex_1.7, density=dens[findInterval(z, brks, all.inside=TRUE)])
}
\keyword{classes}
