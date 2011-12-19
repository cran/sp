\name{is.projected}
\alias{is.projected}
\alias{proj4string}
\alias{proj4string<-}
\alias{is.projected,Spatial-method}
\alias{proj4string,Spatial-method}
\alias{proj4string<-,Spatial,CRS-method}
\alias{proj4string<-,Spatial,character-method}
\alias{get_ll_warn}
\alias{get_ll_TOL}
\alias{set_ll_warn}
\alias{set_ll_TOL}

\title{ Sets or retrieves projection attributes on classes extending
SpatialData }
\description{ Sets or retrieves projection attributes on classes extending
SpatialData; set or retrieve option value for error or warning on exceedance of geographical coordinate range, set or retrieve option value for exceedance tolerance of geographical coordinate range}
\usage{
is.projected(obj)
proj4string(obj)
proj4string(obj) <- value
get_ll_warn()
get_ll_TOL()
set_ll_warn(value)
set_ll_TOL(value)
}
\arguments{
  \item{obj}{An object of class or extending \link{Spatial-class}}
  \item{value}{For \code{proj4string} CRS object, containing a valid proj4 string; attempts to assign an object containing "longlat" to data extending beyond longitude [-180, 360] or lattitude [-90, 90] will be stopped. For \code{set_ll_warn} a single logical value, if FALSE (default) error on range exceedance, if TRUE, warning. For \code{set_ll_TOL} the value of the power of \code{.Machine$double.eps} (default 0.25) to use as tolerance in testing range exceedance}
}

\value{ \code{is.projected} returns a logical; \code{proj4string}
returns a character vector of length 1; \code{spatial.dimension}
returns the number of spatial dimensions (2 or 3).  }

\details{ proj4 strings are operative through CRAN package rgdal. For strings defined as \dQuote{longlat}, the minimum longitude should be -180, the maximum longitude 360, the minimum latitude -90, and the maximum latitude 90}

\author{
  Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}
}

\seealso{ \link{CRS} }

%\examples{}

\keyword{spatial}
