\name{spplot}
\alias{spplot}
\alias{spplot.grid}
\alias{spplot.rings}
\alias{spplot.points}
\alias{layout.north.arrow}
\alias{layout.scale.bar}
\alias{mapLegendGrob}
\alias{sp.theme}

\title{Lattice (trellis) plots for spatial data}
\description{plots for points, grids, polygons }
\usage{
spplot(obj, ...)
spplot.grid(obj, zcol = names(obj), ..., names.attr, 
	scales = list(draw = FALSE), xlab = "", ylab = "", aspect = mapasp(obj), 
	panel = panel.gridplot, sp.layout = NULL, formula)
spplot.rings(obj, zcol = names(obj), ..., names.attr, 
	scales = list(draw = FALSE), xlab = "", ylab = "", aspect = mapasp(obj), 
	panel = panel.ringsplot, sp.layout = NULL, formula)
spplot.points(obj, zcol = names(obj), ..., names.attr, 
	scales = list(draw = FALSE), xlab = "", ylab = "", aspect = mapasp(obj), 
	panel = panel.pointsplot, sp.layout = NULL, identify = FALSE, formula)
mapLegendGrob(obj, widths = unit(1, "cm"), heights = unit(1, "cm"),
	fill = "black", just = "right")
sp.theme()
layout.north.arrow()
layout.scale.bar(height = 0.05)
}
\arguments{
\item{obj}{ object of class extending \link{Spatial-class}} 
\item{zcol}{ character; attribute name(s) or column number(s) in attribute table }
\item{names.attr}{ names to use in panel, if different from zcol names }
\item{scales}{ scales argument to be passed to Lattice plots; use
\code{list(draw = TRUE)} to draw axes scales; see \link{xyplot} for full options }
\item{...}{ other arguments passed to \link{levelplot} (grids, polygons) or 
\link{xyplot} (points) }
\item{xlab}{label for x-axis }
\item{ylab}{label for y-axis }
\item{aspect}{aspect ratio for spatial axes; defaults to "iso" (one unit
on the x-axis equals one unit on the y-axis) but may be set to more
suitable values if the data are e.g. if coordinates are latitude/longitude }
\item{panel}{ depending on the class of obj, \link{panel.ringsplot} (for
rings or lines), \link{panel.gridplot} (grids) or \link{panel.pointsplot}
(points) is used; for further control custom panel functions can be
supplied that call one of these panel functions, but do read how the
argument \code{sp.layout} may help }
\item{sp.layout}{NULL or list; see notes below}
\item{identify}{ if not FALSE, identify plotted objects (currently only working
for points plots). Labels for identification are the row.names of the
attribute table \code{row.names(as.data.frame(obj))}. If TRUE, identify
on panel \code{(1,1)}; for identifying on panel \code{i,j}, pass the
value \code{c(i,j)}}
\item{formula}{optional; may be useful to plot a transformed value. Defaults
to \code{z~x+y} for single and \code{z~x+y|name} for multiple attributes; use
e.g. \code{exp(x)~x+y|name} to plot the exponent of the z-variable}
\item{widths}{width of grob}
\item{heights}{heights of grob}
\item{fill}{fill color of grob}
\item{just}{grob placement justification}
\item{height}{height of scale bar; width is 1.0}
}
\value{
spplot returns a lattice plot of class "trellis", if you fail to "see" it,
explicitly call \code{print(spplot(...))}. If \code{identify} is TRUE,
the plot is plotted and the return value is a vector with row names of
the selected points.

}
\note{
\code{spplot.grid}, \code{spplot.rings} and \code{spplot.points} are S4
methods for \code{spplot}; see \link{spplot-methods}.

Useful arguments that can be passed as \code{...} are: 
\describe{
\item{\code{layout}}{for the layout of panels}
\item{\code{col.regions}}{to specify fill colours}
\item{\code{pretty}}{ for colour breaks at pretty numbers}
\item{\code{at}}{to specify at which values colours change}
\item{\code{as.table}}{to start drawing panels upper-left instead
of lower-left}
\item{\code{page}}{to add marks to each plotted page}
}

for useful values see the appropriate documentation of \link{xyplot}
and \link{levelplot}.

If \code{obj} is of SpatialPointsDataFrame, the following options are
useful to pass:
\describe{
\item{\code{key.space}}{ character: "bottom", "right", "left" or "right" to denote
key location, or list: see argument key in the help for \link{xyplot} what
the options are}
\item{\code{legendEntries}}{ character; array with key legend (text) entries; 
suitable defaults obtained from data }
\item{\code{cuts}}{number of cuts or the actual cuts to use}
\item{\code{do.log}}{logical; if TRUE use log-linear scale to divide range in equal cuts,
else use a linear scale if \code{cuts} is only number of cuts }
\item{\code{pch}}{ integer; plotting character to use; defaults to 16 if fill is TRUE, else 1 }
\item{\code{cex}}{numeric; character expansion, proportional to default value of 1}
\item{\code{fill}}{ logical; use filled circles? }
}

\code{layout.north.arrow} and \code{layout.scale.bar} can be used
to set a north arrow or scale bar.

The \code{sp.layout} argument is either a single layout item, or a list
with a layout items. A layout item is a list with its first argument
the name of the layout function to be called: \code{sp.points} for
SpatialPoints, \code{sp.polygon} for SpatialRings object, \code{sp.lines}
for a SpatialLines object, and \code{sp.text} for text to place. The
second argument contains the object (or text) to be plotted; remaining
arguments are passed to the corresponding \code{panel.*} functions.

A special layout list item is \code{which} (integer), to control to which
panel a layout item should be added. If \code{which} is present in the
main, top-level list it applies for all layout items; in sub-lists with
layout items it denotes the (set of) panels in which the layout item
should be drawn.  Without a \code{which} item, layout items are drawn
in each panel.

The order of items in \code{sp.layout} matters; objects are drawn in
the order they appear. Plot order and prevalence of sp.layout items:
for points and lines, sp.layout items are drawn before the points
(to allow for grids and polygons); for grids and polygons sp.layout
is drawn afterwards (so the item will not be overdrawn by the grid
and/or polygon). Although a matter of taste, transparency may help when
combining things.

\code{sp.theme} returns a lattice theme; use \code{trellis.par.set(sp.theme())}
after a device is opened or changed to make this work. Currently, this only
sets the colors to \link{bpy.colors}.
}

\references{
\url{http://r-spatial.sourceforge.net/}
}
\seealso{
}
\author{
Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl}
}
\examples{
library(lattice)
trellis.par.set(sp.theme()) # sets bpy.colors() ramp
data(meuse)
coordinates(meuse) <- ~x+y
l2 = list("SpatialRingsRescale", layout.north.arrow(), offset = c(181300,329800), 
	scale = 400)
l3 = list("SpatialRingsRescale", layout.scale.bar(), offset = c(180500,329800), 
	scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329900), "0")
l5 = list("sp.text", c(181000,329900), "500 m")

spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5),col.regions="black",pch=c(1,2,3),
	key.space=list(x=0.1,y=.95,corner=c(0,1)))
spplot(meuse, c("zinc", "lead"), sp.layout=list(l2,l3,l4,l5, which = 2),
	key.space=list(x=0.1,y=.95,corner=c(0,1)))

if (require(RColorBrewer)) {
	spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5),
		col.regions=brewer.pal(3, "Set1"))
}
}
\keyword{dplot}
