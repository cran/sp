\name{Sline-class}
\docType{class}
\alias{Sline-class}
% \alias{coordinates,Sline-method}

\title{Class "Sline" }
\description{ class for line objects }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Sline", ...)},
or (preferred) by calls to the function \link{Sline}
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"}, containing the
	line coordinates }
    \item{\code{bbox}:}{Object of class \code{"matrix"}, see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}, see \link{CRS-class} }
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
  \describe{
    \item{coordinates}{\code{signature(obj = "Sline")}: retrieve coordinates
	from line }
    \item{lines}{\code{signature(x = "Sline")}: add lines to a plot }
  }
}
% \references{  }
\author{ Roger Bivand, Edzer Pebesma }
% \note{ }

\seealso{
  \link{Slines-class}, \link{SpatialLines-class}
}
\examples{
}
\keyword{classes}
