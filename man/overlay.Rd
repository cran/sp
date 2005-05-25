\name{overlay}
\alias{overlay}
\title{ spatial overlay for points, grids and rings }
\description{
}
\usage{
overlay(x, y, ...)
}
\arguments{
\item{x}{first layer}
\item{y}{second layer, put on top of x}
\item{...}{ optional arguments; see example below }
}
\value{ 
a numerical array of indices of \code{x} on locations of \code{y}, or
a data.frame with (possibly aggregate) properties of \code{x} in units
of \code{y}.
}
\note{points on a ring boundary and points corresponding to a ring vertex are
considered to be inside the ring}
\seealso{\link{overlay-methods}, \link{point.in.polygon}}
\examples{
r1 = cbind(c(180114, 180553, 181127, 181477, 181294, 181007, 180409, 
180162, 180114), c(332349, 332057, 332342, 333250, 333558, 333676, 
332618, 332413, 332349))
r2 = cbind(c(180042, 180545, 180553, 180314, 179955, 179142, 179437, 
179524, 179979, 180042), c(332373, 332026, 331426, 330889, 330683, 
331133, 331623, 332152, 332357, 332373))
r3 = cbind(c(179110, 179907, 180433, 180712, 180752, 180329, 179875, 
179668, 179572, 179269, 178879, 178600, 178544, 179046, 179110),
c(331086, 330620, 330494, 330265, 330075, 330233, 330336, 330004, 
329783, 329665, 329720, 329933, 330478, 331062, 331086))

sr1=Srings(list(Sring(r1)),"r1")
sr2=Srings(list(Sring(r2)),"r2")
sr3=Srings(list(Sring(r3)),"r3")
sr=SpatialRings(list(sr1,sr2,sr3))
srdf=SpatialRingsDataFrame(sr, data.frame(cbind(1:3,5:3), row.names=c("r1","r2","r3")))

data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE

plot(meuse)
polygon(r1)
polygon(r2)
polygon(r3)

overlay(srdf, meuse)
overlay(sr, meuse)

overlay(meuse, srdf, fn = mean)
overlay(meuse, srdf)
overlay(as(meuse, "SpatialPoints"), srdf)
overlay(as(meuse, "SpatialPoints"), sr)

# same thing, with grid:
overlay(srdf, meuse.grid)
overlay(sr, meuse.grid)

overlay(meuse.grid, srdf, fn = mean)
overlay(meuse.grid, srdf)
overlay(as(meuse.grid, "SpatialPoints"), srdf)
overlay(as(meuse.grid, "SpatialPoints"), sr)
}
\keyword{manip}
