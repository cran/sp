\name{SpatialLines}
\alias{SpatialLines}
\alias{SpatialLinesDataFrame}
\alias{as.SpatialLines.SLDF}
\alias{getSLlinesSlot}
\alias{getSLLinesIDSlots}
\title{ create objects of class SpatialLines or SpatialLinesDataFrame}
\description{ create objects of class \code{SpatialLines} or 
\code{SpatialLinesDataFrame} from
lists of \code{Lines} objects and data.frames; extract list od Lines from a SpatialLines object}
\usage{ 
SpatialLines(LinesList, proj4string = CRS(as.character(NA)))
SpatialLinesDataFrame(sl, data, match.ID = TRUE)
as.SpatialLines.SLDF(SLDF)
getSLlinesSlot(SL)
getSLLinesIDSlots(SL)
}
\arguments{
\item{LinesList}{ list with objects of class \link{Lines-class}}
\item{proj4string}{Object of class \code{"CRS"}; holding a valid proj4 string}

\item{sl, SL}{ object of class \link{SpatialLines-class}}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data}
should equal the number of \code{Lines} elements in \code{sl}}
\item{match.ID}{logical: (default TRUE): match SpatialLines member Lines ID slot values with data frame row names, and re-order the data frame rows if necessary}
\item{SLDF}{SpatialLinesDataFrame object}
}
\value{
\code{SpatialLines} returns object of class \code{SpatialLines};
\code{SpatialLinesDataFrame} returns object of class \code{SpatialLinesDataFrame}
}
\seealso{ \link{SpatialLines-class} }

\keyword{manip}
