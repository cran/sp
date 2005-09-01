\name{SpatialLines-class}
\docType{class}
\alias{SpatialLines-class}
% \alias{coordinates,SpatialLines-method}
\alias{[,SpatialLines-method}
\alias{plot,SpatialLines,missing-method}
\alias{summary,SpatialLines-method}

\title{a class for spatial lines }
\description{ a class that holds spatial lines }
\section{Objects from the Class}{
hold a list of Lines objects; each Lines object holds a list
of Line (line) objects.
}
\section{Slots}{
  \describe{
    \item{\code{lines}:}{Object of class \code{"list"}; list members are all of
	class \link{Lines-class}}
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
	\item{plot}{\code{\signature(x = "SpatialLines", y = "missing")}: 
	plot lines in SpatialLines object}
	\item{lines}{\code{\signature(x = "SpatialLines")}: 
	add lines in SpatialLines object to a plot }
	\item{summary}{\code{\signature(object = "SpatialLines")}: 
	summarize object }
  }
}
\references{ }
\author{ Roger Bivand, Edzer Pebesma }
\note{ }

\seealso{
	\link{Line-class}, \link{Lines-class}
}
\examples{
}
\keyword{classes}
