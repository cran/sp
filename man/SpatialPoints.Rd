\name{SpatialPoints}
\alias{SpatialPoints}
\alias{SpatialPointsDataFrame}
\title{ create objects of class SpatialPoints or SpatialPointsDataFrame}
\description{ create objects of class \link{SpatialPoints-class} or 
\link{SpatialPointsDataFrame-class} from
coordinates, and from coordinates and \code{data.frame}s}
\usage{ 
SpatialPoints(coords, proj4string=CRS(as.character(NA)), bbox = NULL)
SpatialPointsDataFrame(coords, data, coords.nrs = numeric(0), 
      proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox = NULL)

}
\arguments{
\item{coords}{numeric matrix or data.frame with coordinates 
(each row is a point); in case of SpatialPointsDataFrame an object
of class \link{SpatialPoints-class} is also allowed}
\item{proj4string}{ projection string of class \link{CRS-class}}
\item{bbox}{bounding box matrix, usually NULL and constructed from the data, but may be passed through for coercion purposes if clearly needed}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data} 
should equal the number of points in the \code{coords} object}
\item{coords.nrs}{numeric; if present, records the column positions where
in \code{data} the coordinates were taken from (used by \link{coordinates<-})}
\item{match.ID}{logical; if TRUE AND coords has rownames (i.e., coerced
to a matrix, \code{dimnames(coords)[[2]]} is not NULL), AND data has
row.names (i.e. is a data.frame), then the \code{SpatialPointsDataFrame}
object is formed by matching the row names of both components, leaving
the order of the coordinates in tact. Checks are done to see whether
both row names are sufficiently unique, and all data are matched.
If FALSE, coordinates and data are simply "glued" together. If
character: indicates the column in \code{data} with coordinates IDs
to match}
}
\value{
\code{SpatialPoints} returns an object of class \code{SpatialPoints};
\code{SpatialPointsDataFrame} returns an object of class \code{SpatialPointsDataFrame};
}
\seealso{ \link{coordinates}, \link{SpatialPoints-class},
\link{SpatialPointsDataFrame-class} }
\keyword{manip}
