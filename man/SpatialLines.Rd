\name{SpatialLines}
\alias{SpatialLines}
\alias{SpatialLinesDataFrame}
\alias{as.SpatialLines.SLDF}
\alias{getSLlinesSlot}
\alias{getSLSlinesIDSlots}
\title{ create objects of class SpatialLines or SpatialLinesDataFrame}
\description{ create objects of class \code{SpatialLines} or 
\code{SpatialLinesDataFrame} from
lists of \code{Slines} objects and data.frames; extract list od Slines from a SpatialLines object}
\usage{ 
SpatialLines(SlineList)
SpatialLinesDataFrame(sl, data, match.ID = TRUE)
as.SpatialLines.SLDF(SLDF)
getSLlinesSlot(SL)
getSLSlinesIDSlots(SL)
}
\arguments{
\item{SlineList}{ list with objects of class \link{Slines-class}}
\item{sl, SL}{ object of class \link{SpatialLines-class}}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data}
should equal the number of \code{Slines} elements in \code{sl}}
\item{match.ID}{logical: (default TRUE): match SpatialRings member Srings ID slot values with data frame row names, and re-order the data frame rows if necessary}
\item{SLDF}{SpatialLinesDataFrame object}
}
\value{
\code{SpatialLines} returns object of class \code{SpatialLines};
\code{SpatialLinesDataFrame} returns object of class \code{SpatialLinesDataFrame}
}
\seealso{ \link{SpatialLines-class} }

\keyword{manip}
