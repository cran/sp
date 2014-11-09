\name{aggregate}
\docType{methods}
\alias{aggregate}
\alias{aggregate.Spatial}

\title{ aggregation of spatial objects }
\description{ spatial aggregation of thematic information in spatial objects}
\usage{
\method{aggregate}{Spatial}(x, by, FUN = mean, \dots, dissolve = TRUE)
}
\arguments{
\item{x}{object deriving from \link{Spatial}, with attributes }
\item{by}{ aggregation predicate; if \code{by} is a \link{Spatial} object,
the geometry over which attributes in \code{x} are aggregated; if \code{by}
is a list, aggregation by attribute(s), see \link{aggregate.data.frame}}
\item{FUN}{aggregation function }
\item{...}{arguments passed on to function \code{FUN}}
\item{dissolve}{logical; should, when aggregating based on attributes, the
resulting geometries be dissolved? Note that if \code{x} has class
\code{SpatialPointsDataFrame}, this is not possible}
}
\value{ 
Function aggregate.Spatial aggregates the attribute values of
\code{x} either over the geometry of \code{by} by using \link{over} for
spatial matching, or by attribute values, using aggregation function 
\code{FUN}.
}

\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}

\note{ uses \link{over} to find spatial match if \code{by} is a 
\link{Spatial} object }

\examples{
data("meuse")
coordinates(meuse) <- ~x+y
data("meuse.grid")
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
i = cut(meuse.grid$dist, c(0,.25,.5,.75,1), include.lowest = TRUE)
j = sample(1:2, 3103,replace=TRUE)
\dontrun{
if (require(rgeos)) {
	# aggregation by spatial object:
	ab = gUnaryUnion(as(meuse.grid, "SpatialPolygons"), meuse.grid$part.a)
	x = aggregate(meuse["zinc"], ab, mean)
	spplot(x)
	# aggregation by attribute, then dissolve to polygon:
	x = aggregate(meuse.grid["dist"], list(i=i))
	spplot(x["i"])
	x = aggregate(meuse.grid["dist"], list(i=i,j=j))
	spplot(x["dist"], col.regions=bpy.colors())
	spplot(x["i"], col.regions=bpy.colors(4))
	spplot(x["j"], col.regions=bpy.colors())
}
}

x = aggregate(meuse.grid["dist"], list(i=i,j=j), dissolve = FALSE)
spplot(x["j"], col.regions=bpy.colors())

if (require(gstat) && require(rgeos)) {
	x = idw(log(zinc)~1, meuse, meuse.grid, debug.level=0)[1]
	spplot(x[1],col.regions=bpy.colors())
	i = cut(x$var1.pred, seq(4, 7.5, by=.5), 
		include.lowest = TRUE)
	xa = aggregate(x["var1.pred"], list(i=i))
	spplot(xa[1],col.regions=bpy.colors(8))
}
}
\keyword{methods}
