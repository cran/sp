\name{image.SpatialGridDataFrame}
\alias{image.SpatialGridDataFrame}
\alias{as.image.SpatialGridDataFrame}
\title{ image gridded spatial data, or convert to format for image }
\description{
Convert gridded data in SpatialGridDataFrame to image format; call
image on data in SpatialGridDataFrame format
}
\usage{
image.SpatialGridDataFrame(x, attr = 1, xcol = 1, ycol = 2, asp = 1, xlab, ylab, ...)
as.image.SpatialGridDataFrame(x, xcol = 1, ycol = 2)
}
\arguments{
\item{x}{ object of class \link{SpatialGridDataFrame} }
\item{attr}{ column number of attribute variable; this may be
the column name in the data.frame of \code{data} (as.data.frame(data)), or
a column number }
\item{xcol}{ column number of x-coordinate, in the coordinate matrix }
\item{ycol}{ column number of y-coordinate, in the coordinate matrix }
\item{asp}{ aspect ratio of unit x and unit y axis }
\item{xlab}{ label for x-axis }
\item{ylab}{ label for y-axis }
\item{...}{ arguments passed to \link{image}, see examples }
}
\value{ \code{as.image.SpatialGridDataFrame} returns the list with
elements \code{x} and \code{y}, containing the coordinates of the cell
centres of a matrix \code{z}, containing the attribute values in matrix
form as needed by \link{image}. }

\note{ Providing \code{xcol} and \code{ycol} attributes seems obsolete,
and it is for 2D data, but it may provide opportunities for plotting
certain slices in 3D data. I haven't given this much thought yet.

\link{filled.contour} seems to misinterpret the coordinate values,
if we take the image.default manual page as the reference. }

\references{ }

\author{ Edzer J. Pebesma }

\seealso{ \link{image.default}, \link{SpatialGridDataFrame-class},
\link{levelplot} in package \code{lattice} }

\examples{
data(meuse.grid)
coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(meuse.grid) = TRUE            # promote to SpatialGridDataFrame
image(meuse.grid["dist"], main = "Distance to river Meuse")
data(meuse)
coordinates(meuse) = c("x", "y")
points(coordinates(meuse), pch = "+")
}

\keyword{spatial}
