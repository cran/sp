\name{nowrapSpatialLines}
\alias{nowrapSpatialLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split SpatialLines components at offset}
\description{
  When recentering a world map, most often from the "Atlantic" view with longitudes with range -180 to 180, to the "pacific" view with longitudes with range 0 to 360, lines crossing the offset (0 for this conversion) get stretched horizonally. This function breaks Line objects at the offset (usually Greenwich), inserting a very small gap, and reassembling the Line objects created as Lines. A similar function for polygons is found in the \pkg{spgpc} package.
}
\usage{
nowrapSpatialLines(obj, offset = 0, eps = rep(.Machine$double.eps, 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{A Spatial Lines object}
  \item{offset}{default 0, untried for other values}
  \item{eps}{vector of two fuzz values, both default double.eps}
}

\value{
  A Spatial Lines object
}

\author{Roger Bivand}

\examples{
Sl <- SpatialLines(list(Lines(list(Line(cbind(sin(seq(-4,4,0.4)),
 seq(1,21,1)))), "1")), proj4string=CRS("+proj=longlat"))
summary(Sl)
nwSL <- nowrapSpatialLines(Sl)
summary(nwSL)
}
\keyword{spatial}
