\name{SpatialPointsDataFrame-class}
\docType{class}
\alias{SpatialPointsDataFrame-class}
\alias{[,SpatialPointsDataFrame-method}
\alias{coerce,SpatialPointsDataFrame,data.frame-method}
\alias{coerce,SpatialPointsDataFrame,SpatialPoints-method}
\alias{show,SpatialPointsDataFrame-method}
% \alias{coordinates,SpatialPointsDataFrame-method}
% \alias{coordinates}
\alias{SpatialPointsDataFrame}
\alias{"[[.SpatialPointsDataFrame"}
\alias{"[[<-.SpatialPointsDataFrame"}
\alias{"$.SpatialPointsDataFrame"}
\alias{"$<-.SpatialPointsDataFrame"}
\alias{as.data.frame.SpatialPointsDataFrame}
\alias{dim.SpatialPointsDataFrame}
\alias{plot.SpatialPointsDataFrame}
\alias{print.SpatialPointsDataFrame}
\alias{summary.SpatialPointsDataFrame}
\alias{print.summary.SpatialPointsDataFrame}
\alias{ShowSpatialPointsDataFrame}
\alias{as.SpatialPoints.SpatialPointsDataFrame}

\title{Class "SpatialPointsDataFrame" ~~~ }
\description{ Class for spatial attributes that have spatial point locations }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{coordinates(x) = c("x", "y") }.
or of the form \code{coordinates(x) = xy}; see \link{coordinates}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}; the attribute data,
	which may or may not contain the coordinates in the columns }
    \item{\code{coords}:}{Object of class \code{"matrix"}; the coordinates matrix
	(points are rows in the matrix) }
	\item{\code{coords.stripped}}{Object of class logical; if TRUE, when
	the object was created the coordinates were retrieved from the data.frame,
	and hence stripped from it; after coercion to data.frame, e.g. by 
	as.data.frame(x), coordinates will again be added (as first few columns) 
	to the data.frame }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection string}
  }
}
\section{Extends}{
Class \code{"SpatialPoints"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialPoints"}.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialPointsDataFrame")}: subset rows or
	columns; in case of row subsetting, the coordinates are also subsetted }
    \item{coerce}{\code{signature(from = "SpatialPointsDataFrame", to = "data.frame")}:      extracts that data.frame part  }
    \item{coordinates}{\code{signature(obj = "SpatialPointsDataFrame")}: retrieves
	the coordinates only }
    \item{show}{\code{signature(object = "SpatialPointsDataFrame")}: print method }
    \item{plot}{\code{signature(x = "SpatialPointsDataFrame")}: plot points}
    \item{points}{\code{signature(x = "SpatialPointsDataFrame")}: add points to plot}
  }
}
\references{  }
\author{ Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl} }
\note{  }

\seealso{
  \code{\link{coordinates}}, \code{\link{SpatialPoints-class}} 
}
\examples{
data(meuse)
xy = meuse[c("x", "y")] # retrieve coordinates as data.frame
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = c("x", "y") # specify column names
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = c(1, 2) # specify column names
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = ~x+y # formula
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = xy   # as data frame
class(meuse)
data(meuse) # reload data.frame
coordinates(meuse) = as.matrix(xy)   # as matrix
meuse$log.zn = log(meuse$zinc)
class(meuse)
dim(meuse)
}
\keyword{classes}
