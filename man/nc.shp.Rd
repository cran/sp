\name{nc.shp}
\alias{nc.shp}
\docType{data}
\title{North Carolina county boundaries (shapefile)}
\description{
  North Carolina county boundaries in shapefile (ArcView and ArcGIS) format for counties from the 1990 census.
}
\usage{data(ncshp)}
\format{
  The format is as read by \code{read.shape} in the maptools package
}

\source{
  \url{http://sal.agecon.uiuc.edu/stuff/data.html}
}

\examples{
\dontrun{
library(maptools)
nc.shp <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
}
data(ncshp)
nc1 <- as.SpatialRings.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
plotSpatialRings(nc1)
invisible(text(getSRSringsLabptSlots(nc1), labels=getSRSringsIDSlots(nc1), cex=0.6))
}
\keyword{datasets}
