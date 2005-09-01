\name{spsample}
\alias{spsample}
\alias{makegrid}
\alias{sample.Spatial}
\alias{sample.Line}
\alias{sample.Polygon}
\alias{sample.Polygons}
\alias{sample.Sgrid}
\title{ sample point locations within a spatial region }
\description{ sample point locations within a square area, a grid,
a polygon, or on a spatial line, using regular or random sampling
}
\usage{
spsample(x, n, type, ...)
sample.Spatial(x, n, type, bb = bbox(x), offset = runif(2), cellsize, ...)
sample.Line(x, n, type, offset = runif(1), proj4string=CRS(as.character(NA)), ...)
sample.Polygon(x, n, type = "random", bb = bbox(x), offset = runif(2), proj4string=CRS(as.character(NA)), ...)
sample.Polygons(x, n, type = "random", bb = bbox(x), offset = runif(2), proj4string=CRS(as.character(NA)), ...)
sample.Sgrid(x, n, type = "random", bb = bbox(x), offset = runif(2), ...)
makegrid(x, n = 10000, nsig = 2, cellsize, offset = c(0.5,0.5), 
	type = "regular", ...)
}
\arguments{
\item{x}{Spatial object; \code{spsample(x,...)} is a generic method for the
existing \code{sample.Xxx} fumctions}
\item{...}{ optional arguments, passed to the appropriate \code{sample.Xxx}
functions}
\item{n}{ (approximate) sample size }
\item{type}{ character; \code{"random"} for completely spatial random;
\code{"regular"} for regular (systematically aligned) sampling;
\code{"stratified"} for stratified random (one single random location in
each "cell"); or \code{"nonaligned"} for nonaligned systematic sampling
(nx random y coordinates, ny random x coordinates) }
\item{bb}{ bounding box of the sampled domain; setting this to a smaller
value leads to sub-region sampling }
\item{offset}{ for regular sampling only: the offset (position) of the regular
grid; the default for \code{spsample} methods is a random location in
the unit cell $[0,1] \times [0,1]$, leading to a different grid after
each call; if this is set to \code{c(0.5,0.5)}, the returned grid is
not random (but, in Ripley's wording, "centric systematic") }
\item{cellsize}{ if missing, a cell size is derived from the sample size
\code{n}; otherwise, this cell size is used for all sampling methods
except \code{"random"} }
  \item{proj4string}{Object of class \code{"CRS"};
	holding a valid proj4 string}
\item{nsig}{ for "pretty" coordinates; \code{spsample} do not result in
pretty grids }
}
\value{ an object of class \link{SpatialPoints-class}. The number of
points is only guaranteed to equal \code{n} when sampling is done in a
square box, i.e. (\code{sample.Spatial}). Otherwise, the obtained number
of points will have expected value \code{n}. 

When \code{x} is of a class deriving from \link{Spatial-class} for which
no \link{spsample-methods} exists, sampling is done in the bounding box
of the object, using \code{spsample.Spatial}. An \link{overlay} may be
necessary to select afterwards. 

Sampling type \code{"nonaligned"} is not implemented for line objects. }

\note{If an \link{Polygon-class} object has zero area (i.e. is a line),
samples on this line element are returned. If the area is very close
to zero, the algorithm taken here (generating points in a square area,
selecting those inside the polygon) may be very resource intensive }

\references{Chapter 3 in B.D. Ripley, 1981. Spatial Statistics, Wiley}
\author{Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl}}

\seealso{\link{overlay-methods}, \link{point.in.polygon}, \link{sample}}
\examples{

data(meuse.riv)
meuse.sr = SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)), "x")))

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "regular"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "random"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "stratified"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr, n = 1000, "nonaligned"), pch = 3)

plot(meuse.sr)
points(spsample(meuse.sr@polygons[[1]], n = 100, "stratified"), pch = 3, cex=.5)

data(meuse.grid)
gridded(meuse.grid) = ~x+y
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="random"), pch=3, cex=.5)
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="stratified"), pch=3, cex=.5)
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="regular"), pch=3, cex=.5)
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="nonaligned"), pch=3, cex=.5)

fullgrid(meuse.grid) = TRUE
image(meuse.grid)
points(spsample(meuse.grid,n=1000,type="stratified"), pch=3,cex=.5)

}
\keyword{manip}
