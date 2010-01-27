\name{is.projected}
\alias{is.projected}
\alias{proj4string}
\alias{proj4string<-}

\title{ Sets or retrieves projection attributes on classes extending
SpatialData }
\description{ Sets or retrieves projection attributes on classes extending
SpatialData }
\usage{
is.projected(sd)
proj4string(sd)
proj4string(sd) <- value
}
\arguments{
  \item{sd}{An object of class or extending \link{Spatial-class}}
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
