\name{panel.spplot}
\alias{panel.spplot}
\alias{panel.gridplot}
\alias{panel.pointsplot}
\alias{panel.ringsplot}
\alias{sp.pagefn} % needs docs!
\alias{spplot.key} % needs docs!
\alias{SpatialRingsRescale}
\alias{sp.lines}
\alias{sp.points}
\alias{sp.polygon}
\alias{sp.text}

\title{panel and panel utility functions for spplot}
\description{panel functions for spplot functions, and functions that can be
useful within these panel functions }
\usage{
sp.pagefn(n)
spplot.key(sp.layout, rows = 1, cols = 1)
SpatialRingsRescale(obj, offset, scale = 1, fill = "black", col = "black",...)
sp.lines(obj, col = 1, ...)
sp.points(obj, pch = 3, ...)
sp.polygon(obj, col = 1, ...)
sp.text(loc, txt, ...)
}
\arguments{
\item{n}{ignored}
\item{sp.layout}{ list; see \link{spplot} for definition }
\item{rows}{ integer; panel row(s) for which the layout should be drawn }
\item{cols}{ integer; panel column(s) for which the layout should be drawn }
\item{obj}{ object of class \link{SpatialRings-class} for
\code{SpatialRingsRescale}; of class \link{SpatialLines-class},
\link{Slines-class} or \link{Sline-class} for \code{sp.lines} of a
class that has a \link{coordinates-methods} for \code{sp.points}; of
class \link{SpatialRings-class} for \code{sp.polygon}.  When obj is
character, the actual object is retrieved by \code{get(obj)} before its
class is evaluated.  }
\item{offset}{ offset for shifting a Rings object }
\item{scale}{ scale for rescaling }
\item{fill}{ fill color }
\item{col}{ line color }
\item{pch}{ plotting character }
\item{loc}{ numeric vector of two elements }
\item{txt}{ text to be plotted }
\item{...}{ arguments passed to the underlying lattice or grid functions}
}
\note{
The panel functions of \link{spplot}, \code{panel.gridplot} for grids,
\code{panel.pointsplot} for points, or \code{panel.ringsplot} for lines
or rings (polygons) can be called with arguments \code{(x,y,...)}.
Customizing spplot plots can be done by extending the panel function,
or by supplying an sp.layout argument; see the documentation for
\link{spplot}.

\code{SpatialRingsRescale} scales and shifts an object of class
\link{SpatialRings-class}; this is useful e.g. for scale bars, or other
layout items.

\code{sp.lines}, \code{sp.points}, \code{sp.polygon} and \code{sp.text}
plot lines, points, rings or text in a panel.

\code{spplot.key} draws the \code{sp.layout} object at given rows/cols.

\code{sp.pagefn} can be passed as a \code{page} argument, and will
call function \code{spplot.key} for the last panel drawn on a page.
}

\references{
\url{http://r-spatial.sourceforge.net/} has a graph gallery with
examples with R code.
}
\seealso{\link{spplot}, \link{spplot-methods}}
\author{ Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl} }
\examples{

}
\keyword{dplot}
