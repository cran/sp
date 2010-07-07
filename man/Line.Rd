\name{Line}
\alias{Line}
\alias{Lines}
\title{ create objects of class Line or Lines}
\description{ create objects of class \code{Line} or \code{Lines} from coordinates}
\usage{ 
Line(coords)
Lines(slinelist, ID)
}
\arguments{
\item{coords}{ 2-column numeric matrix with coordinates for a single line }
\item{slinelist}{ list with elements of class \link{Line-class}}
\item{ID}{a unique character identifier}
}
\value{
	\code{Line} returns an object of class \link{Line-class};
	\code{Lines} returns an object of class \link{Lines-class}
}
\seealso{ \link{SpatialLines-class} }

\keyword{classes}
