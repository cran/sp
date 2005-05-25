\name{rings}
\alias{rings}
\alias{rings<-}

\title{ sets spatial coordinates to create spatial data, or retrieves
spatial coordinates }
\description{ sets spatial coordinates to create spatial data, or retrieves
spatial coordinates }
\usage{
	rings(obj)
	rings(object) <- value
}
\arguments{
\item{obj}{ object of class "SpatialRingsDataFrame" }
\item{object}{ object of class "data.frame" }
\item{value}{ object of class "SpatialRings" }
}
\value{ rings returns the SpatiaRings of obj; rings<- promotes a
data.frame to a SpatialRingsDataFrame object }
\examples{
data(ncshp)
nc1 <- as.SpatialRings.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
df <- nc.shp$att.data
rownames(df) <- df$FIPS
rings(df) <- nc1
class(df)
}
\keyword{manip}
