\name{Slines-class}
\docType{class}
\alias{Slines-class}
\alias{getSlinesSlinesSlot}
% \alias{coordinates,Slines-method}

\title{Class "Sline" }
\description{ class for sets of line objects }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{Sline}
}
\section{Slots}{
  \describe{
    \item{\code{Slines}:}{Object of class \code{"list"}, containing elements
	of class \link{Slines-class}}
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
	from lines; returns list with matrices }
    \item{lines}{\code{signature(x = "Sline")}: add lines to a plot}
  }
}
\usage{
getSlinesSlinesSlot(SL)
}
\arguments{
\item{SL}{an Slines object}
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
