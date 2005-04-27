\name{Srings-class}
\docType{class}
\alias{Srings-class}

\title{Class "Srings"}
\description{ Collection of objects of class \code{"Sring"} }
\section{Objects from the Class}{
Objects can be created by calls to the function \code{Srings}
}
\section{Slots}{
  \describe{
    \item{\code{Srings}:}{Object of class \code{"list"}; list with objects
	of class \link{Sring-class} }
    \item{\code{plotOrder}:}{Object of class \code{"integer"}; order in which
	the Sring objects should be plotted, currently by order of decreasing size }
    \item{\code{labpt}:}{Object of class \code{"numeric"}; pair of x, y coordinates giving a label point, the label point of the largest ring component  }
    \item{\code{ID}:}{Object of class \code{"character"}; identifier string  }
    \item{\code{area}:}{Object of class \code{"numeric"}; the total area of the Sring list including the areas of holes; these values are used to make sure that rings of a smaller area are plotted after rings of a larger area }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class} }
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
No methods defined with class "Srings" in the signature.
}
\references{ }
\author{ Roger Bivand }
\note{ By default, single rings (where Srings is a list of length one) are not expected to be holes, but in multiple rings, hole definitions for member rings can be set. Sring objects belonging to an Srings object should either not overlap one-other, or should be fully included (as lakes or islands in lakes). They should not be self-intersecting. Checking of hole FALSE/TRUE status for Srings objects is included in the spgpc wrapper package for gpclib functions, function checkSringsHoles() (currently on sourceforge). }

\seealso{
}
\examples{
}
\keyword{classes}
