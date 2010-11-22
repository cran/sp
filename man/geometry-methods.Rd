\name{geometry-methods}
\docType{methods}
\alias{geometry}
\alias{geometry-methods}
\alias{geometry,Spatial-method}
\alias{geometry,SpatialPointsDataFrame-method}
\alias{geometry,SpatialGridDataFrame-method}
\alias{geometry,SpatialPixelsDataFrame-method}
\alias{geometry,SpatialLinesDataFrame-method}
\alias{geometry,SpatialPolygonsDataFrame-method}
\title{ Methods for retrieving the geometry from a composite (geometry + attributes) object }
\usage{ geometry(obj) }
\arguments{
\item{obj}{object of class Spatial}
}
\description{
geometry retrieves the Spatial* object from a Spatial*DataFrame object
}
\section{Methods}{
\describe{
\item{obj = "Spatial"}{ }
\item{obj = "SpatialPointsDataFrame"}{ }
\item{obj = "SpatialPolygonsDataFrame"}{ }
\item{obj = "SpatialPixelsDataFrame"}{ }
\item{obj = "SpatialGridDataFrame"}{ }
\item{obj = "SpatialLinesDataFrame"}{ }
}
}

\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}
\keyword{methods}
