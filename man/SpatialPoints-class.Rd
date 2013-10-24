\name{SpatialPoints-class}
\docType{class}
\alias{SpatialPoints-class}
\alias{[,SpatialPoints-method}
\alias{coerce,SpatialPoints,matrix-method}
\alias{coerce,SpatialPoints,data.frame-method} % here???
\alias{coerce,SpatialPoints,SpatialPixels-method} % here???
\alias{coerce,SpatialPoints,Line-method}
\alias{coerce,SpatialPoints,Lines-method}
\alias{coerce,SpatialPoints,SpatialLines-method}
\alias{coerce,SpatialPointsDataFrame,SpatialPixelsDataFrame-method} % here???
%\alias{coerce,SpatialPoints,data.frame-method}
%\alias{coerce,data.frame,SpatialPoints-method}
%\alias{coerce,matrix,SpatialPoints-method}
% \alias{coordinates,SpatialPoints-method}
\alias{show,SpatialPoints-method}
%\alias{SpatialPoints}
\alias{as.data.frame.SpatialPoints}
\alias{plot,SpatialPoints,missing-method}
\alias{summary,SpatialPoints-method}
\alias{print.SpatialPoints} 
\alias{summary.SpatialPoints}
\alias{rbind.SpatialPoints}
\alias{$,SpatialPoints-method}
\alias{$<-,SpatialPoints,character-method}
\alias{row.names.SpatialPoints}
\alias{coerce,ppp,SpatialPoints-method}


\title{Class "SpatialPoints" }
\description{  Class for (irregularly spaced) points }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{SpatialPoints(x)}.
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"}, containing the
	coordinates (each row is a point) }
    \item{\code{bbox}:}{Object of class \code{"matrix"}, with bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}, projection string }
  }
}
\section{Extends}{
Class \code{"Spatial"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialPoints")}: subsets the points; only
	rows can be subsetted }
    \item{coerce}{\code{signature(from = "SpatialPoints", to = "data.frame")}: 
	retrieves the data part}
    \item{coerce}{\code{signature(from = "SpatialPoints", to = "SpatialPixels")}: equivalent to assigning gridded TRUE for a copy of the object}
    \item{coerce}{\code{signature(from = "SpatialPointsDataFrame", to = "SpatialPixelsDataFrame")}: equivalent to assigning gridded TRUE for a copy of the object}
    \item{coerce}{\code{signature(from = "data.frame", to = "SpatialPoints")}: sets
	coordinates, which may be in a data frame }
    \item{coerce}{\code{signature(from = "matrix", to = "SpatialPoints")}: set
	coordinates, which may be in a matrix }
    \item{coordinates}{\code{signature(obj = "SpatialPoints")}: retrieves
	the coordinates, as matrix }
    \item{plot}{\code{signature(x = "SpatialPoints", y = "missing")}: plot points}
    \item{summary}{\code{signature(object = "SpatialPoints")}: summarize object}
    \item{points}{\code{signature(x = "SpatialPoints")}: add point symbols to plot}
    \item{show}{\code{signature(object = "SpatialPoints")}: prints coordinates }
    \item{rbind}{\code{signature(object = "SpatialPoints")}: rbind-like method }  }
}
\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link{SpatialPointsDataFrame-class}}
}
\examples{
x = c(1,2,3,4,5)
y = c(3,2,5,1,4)
S <- SpatialPoints(cbind(x,y))
S <- SpatialPoints(list(x,y))
S <- SpatialPoints(data.frame(x,y))
S
plot(S)
}
\keyword{classes}
