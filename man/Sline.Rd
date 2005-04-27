\name{Sline}
\alias{Sline}
\alias{Slines}
\title{ create objects of class Sline or Slines}
\description{ create objects of class \code{Sline} or \code{Slines} from coordinates}
\usage{ 
Sline(coords, proj4string)
Slines(slinelist)
}
\arguments{
\item{coords}{ 2-column numeric matrix with coordinates for a single line }
\item{proj4string}{ object of class \link{CRS-class}}
\item{slinelist}{ list with elements of class \link{Sline-class}}
}
\value{
	\code{Sline} returns an object of class \link{Sline-class};
	\code{Slines} returns an object of class \link{Slines-class}
}
\seealso{ \link{SpatialLines-class} }

\keyword{classes}
