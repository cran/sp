\name{Spatial-class}
\docType{class}
\alias{Spatial-class}
\alias{print.summary.Spatial}
\alias{summary,Spatial-method}
\alias{plot,Spatial,missing-method}
% \alias{coerce,SpatialDataFrame,SpatialData-method}

\title{Class "Spatial" ~~~ }
\description{ An abstract class from which useful spatial classes are
derived }
\section{Objects from the Class}{
are never to be generated; only derived classes can be meaningful }
\section{Slots}{
  \describe{
    \item{\code{bbox}:}{Object of class \code{"matrix"}; 2-column matrix
	holding the minimum in first and maximum in second column for the
	x-coordinate (first row), y-coordinate (second row) and optionally,
	for points and grids only, further coordinates. }
    \item{\code{proj4string}:}{Object of class \code{"CRS"};
	holding a valid proj4 string, which can be used for unprojecting
	or reprojecting coordinates; it is initialised to NA. Other strings
	are checked for validity in the spproj package.}
  }
}
\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "Spatial")}: retrieves the bbox element }
    \item{dimensions}{\code{signature(obj = "Spatial")}: retrieves the number
	of spatial dimensions spanned }
    \item{gridded}{\code{signature(obj = "Spatial")}: logical, tells whether
	the data is on a regular spatial grid }
    \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	for spatial objects; does nothing but setting up a plotting region and
	possibly drawing axes.  }
	\item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}
\references{  }
\author{ r-spatial team; Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl}
Roger Bivand, Barry Rowlinson, Virgilio G\'{o}mez-Rubio
}
\note{  }

\section{Warning }{this class is not useful in itself, but all spatial
classes in this package derive from it }

\seealso{
  \code{\link{SpatialPoints-class}},
  \code{\link{SpatialGrid-class}},
%  \code{\link{SpatialPolygons-class}},
%  \code{\link{SpatialArcs-class}},
  \code{\link{SpatialPointsDataFrame-class}},
  \code{\link{SpatialGridDataFrame-class}},
%  \code{\link{SpatialPolygonsDataFrame-class}},
}
\examples{
}

\keyword{classes}
