\name{SpatialRingsDataFrame-class}
\docType{class}
\alias{SpatialRingsDataFrame-class}
\alias{[,SpatialRingsDataFrame-method}
\alias{$.SpatialRingsDataFrame}
\alias{$<-.SpatialRingsDataFrame}
\alias{as.data.frame.SpatialRingsDataFrame}
\alias{coerce,SpatialRingsDataFrame,data.frame-method}

\alias{getSRSringsIDSlots}
\alias{getSRSringsLabptSlots}
\alias{getSRnParts}
\alias{getSRplotOrderSlot}
\alias{getSRpolygonsSlot}
\alias{getSringAreaSlot}
\alias{getSringCoordsSlot}
\alias{getSringLabptSlot}
\alias{getSringHoleSlot}
\alias{getSringsIDSlot}
\alias{getSringsLabptSlot}
\alias{getSringsSringsSlot}
\alias{getSringsplotOrderSlot}

\title{Class "SpatialRingsDataFrame" }
\description{ class to hold polygons with attributes }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{SpatialRingsDataFrame}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}; attribute table }
    \item{\code{polygons}:}{Object of class \code{"list"}; see \link{SpatialRings-class} }
    \item{\code{plotOrder}:}{Object of class \code{"integer"}; see \link{SpatialRings-class} }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class}}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}
\section{Extends}{
Class \code{"SpatialRings"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialRings"}.
}
\section{Methods}{
No methods defined with class "SpatialRingsDataFrame" in the signature.
}
\references{ }
\author{ }
\note{\code{SpatialRingsDataFrame} with default ID matching checks the data frame row names against the Srings ID slots. They must then agree with each other, and be unique (no Srings objects can share IDs); the data frame rows will be re-ordered if needed to match the Srings IDs.}

\seealso{
	\link{SpatialRings-class}
}
\examples{
data(ncshp)
nc1 <- as.SpatialRings.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
df <- nc.shp$att.data
rownames(df) <- as.character(nc.shp$att.data$FIPS)
identical(rownames(df), getSRSringsIDSlots(nc1))
ncSRDF <- SpatialRingsDataFrame(nc1, df)
names(as(ncSRDF, "data.frame"))
identical(rownames(as(ncSRDF, "data.frame")), getSRSringsIDSlots(ncSRDF))
rrt <- as(ncSRDF, "data.frame")$SID74/as(ncSRDF, "data.frame")$BIR74
brks <- quantile(rrt, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
plot.SpatialRings(ncSRDF, col=cols[findInterval(rrt, brks, all.inside=TRUE)])
plot.SpatialRings(ncSRDF, density=dens[findInterval(rrt, brks, all.inside=TRUE)])
}
\keyword{classes}
