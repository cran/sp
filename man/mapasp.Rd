\name{mapasp}
\alias{mapasp}
\title{ Calculate aspect ratio for plotting geographic maps }
\description{ Calculate aspect ratio for plotting geographic maps }
\usage{ mapasp(data) }
\arguments{
\item{data}{ object of class \code{SpatialData}, or a class that extends
\code{SpatialData} such as \code{SpatialDataFrame} or 
\code{SpatialDataFrameGrid}}
}
\value{
	diff(range(y))/diff(range(x)), but then derived from slot(data, "bbox")
}
\seealso{ \link{levelplot} in package \code{lattice}}

\keyword{dplot}
