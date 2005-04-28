\name{SpatialLines-class}
\docType{class}
\alias{SpatialLines-class}
% \alias{coordinates,SpatialLines-method}
\alias{[,SpatialLines-method}

\title{a class for spatial lines }
\description{ a class that holds spatial lines }
\section{Objects from the Class}{
hold a list of Slines objects; each Slines object holds a list
of Sline (line) objects.
}
\section{Slots}{
  \describe{
    \item{\code{lines}:}{Object of class \code{"list"}; list members are all of
	class \link{Slines-class}}
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(obj = "SpatialLines")}: select subset of (sets of) lines}
	coordinates; value is a list of lists with matrices }
	\item{plot}{\code{\signature(x = "SpatialLines")}: 
	plot lines in SpatialLines object}
	\item{lines}{\code{\signature(x = "SpatialLines")}: 
	add lines in SpatialLines object to a plot }
  }
}
\references{ }
\author{ Roger Bivand, Edzer Pebesma }
\note{ }

\seealso{
	\link{Sline-class}, \link{Slines-class}
}
\examples{
}
\keyword{classes}
