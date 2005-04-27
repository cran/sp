\name{bbox-methods}
\docType{methods}
\alias{bbox,Spatial-method}
\alias{bbox}
\title{ retrieve bbox from spatial data}
\description{
retrieves spatial bounding box from spatial data
}
\usage{
	bbox(obj)
}
\arguments{
\item{obj}{ object deriving from class "Spatial" }
}
\section{Methods}{
\describe{
\item{obj = "Spatial"}{ object deriviving from class "Spatial" }
}}
\value{ two-column matrix; the first column has the minimum,
the second the maximum values; rows represent the spatial
dimensions}
\keyword{methods}
\examples{
# just 9 points on a grid:
x <- c(1,1,1,2,2,2,3,3,3)
y <- c(1,2,3,1,2,3,1,2,3)
xy <- cbind(x,y)
S <- SpatialPoints(xy)
bbox(S)

# data.frame
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
bbox(meuse.grid)

}
\keyword{ }
