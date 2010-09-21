\name{is.projected}
\alias{is.projected}
\alias{proj4string}
\alias{proj4string<-}
\alias{is.projected,Spatial-method}
\alias{proj4string,Spatial-method}
\alias{proj4string<-,Spatial,CRS-method}
\alias{proj4string<-,Spatial,character-method}

\title{ Sets or retrieves projection attributes on classes extending
SpatialData }
\description{ Sets or retrieves projection attributes on classes extending
SpatialData }
\usage{
is.projected(obj)
proj4string(obj)
proj4string(obj) <- value
}
\arguments{
  \item{obj}{An object of class or extending \link{Spatial-class}}
  \item{value}{ CRS object, containing a valid proj4 string; attempts to assign an object containing "longlat" to data extending beyond longitude [-180, 360] or lattitude [-90, 90] will be stopped }
}

\value{ \code{is.projected} returns a logical; \code{proj4string}
returns a character vector of length 1; \code{spatial.dimension}
returns the number of spatial dimensions (2 or 3).  }

\details{ proj4 strings are operative through CRAN package rgdal}

\author{
  Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}
}

\seealso{ \link{CRS} }

%\examples{}

\keyword{spatial}
