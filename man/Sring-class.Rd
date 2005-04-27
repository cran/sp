\name{Sring-class}
\docType{class}
\alias{Sring-class}

\title{Class "Sring" }
\description{ class for spatial polygon }
\section{Objects from the Class}{
Objects can be created by calls to the function \code{Sring}
}
\section{Slots}{
  \describe{
    \item{\code{ringDir}:}{Object of class \code{"integer"}; the ring direction of the ring coordinates, holes are expected to be anti-clockwise }
    \item{\code{labpt}:}{Object of class \code{"numeric"}; an x, y coordinate pair forming the label point of the ring }
    \item{\code{area}:}{Object of class \code{"numeric"}; the area of the ring }
    \item{\code{hole}:}{Object of class \code{"logical"}; does the ring seem to be a hole }
    \item{\code{coords}:}{Object of class \code{"matrix"}; coordinates of the polygon;
	first point should equal the last point }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class} }
  }
}
\section{Extends}{
Class \code{"Sline"}, directly.
Class \code{"Spatial"}, by class \code{"Sline"}.
}
\section{Methods}{
No methods defined with class "Sring" in the signature.
}
\references{ }
\author{ Roger Bivand }
\note{ }

\seealso{
	\link{Srings-class}, \link{SpatialRings-class}
}
\examples{
}
\keyword{classes}
