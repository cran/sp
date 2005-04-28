\name{as.SpatialRings.SringsList}
\alias{as.SpatialRings.SringsList}
\alias{as.SpatialRings.Shapes}
\alias{as.SpatialRings.map}
\alias{as.SpatialRings.pal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Making SpatialRings objects}
\description{
  These functions show how SpatialRings objects may be built from data from shapefiles, from the maps package, and from ArcInfo e00 files.
}
\usage{
as.SpatialRings.SringsList(Srl)
as.SpatialRings.Shapes(shapes, IDs, proj4string=CRS(as.character(NA)))
as.SpatialRings.map(map, IDs, proj4string=CRS(as.character(NA)))
as.SpatialRings.pal(arc, pal, IDs, dropPoly1=TRUE, proj4string=CRS(as.character(NA)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Srl}{A list of Srings objects}
  \item{shapes}{The Shapes component of a Map object as read by \code{read.shape}}
  \item{IDs}{Unique character ID values for each output Srings object; the input IDs can be an integer or character vector with duplicates, where the duplicates will be combined as a single output Srings object}
  \item{proj4string}{Object of class \code{"CRS"};
	holding a valid proj4 string}
  \item{map}{Object returned by \code{map} containing polygon boundaries}
  \item{arc}{Object returned by \code{get.arcdata}}
  \item{pal}{Object returned by \code{get.paldata}}
  \item{dropPoly1}{Should the first polygon in the AVC or e00 data be dropped; the first polygon is typically the compound boundary of the whole dataset, and can be detected by looking at the relative lengths of the list components in the second component of pal, which are the numbers of arcs making up the boundary of each polygon}
}

\value{
  The functions return a SpatialRings object
}

\author{ Roger Bivand }

\examples{
\dontrun{
library(maptools)
nc.shp <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
}
data(ncshp)
nc1 <- as.SpatialRings.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
plotSpatialRings(nc1)
invisible(text(getSRSringsLabptSlots(nc1), labels=getSRSringsIDSlots(nc1), cex=0.6))
\dontrun{
library(maps)
ncmap <- map("county", "north carolina", fill=TRUE, col="transparent", plot=FALSE)
}
data(ncmap)
IDs <- sapply(strsplit(ncmap$names, ","), function(x) x[2])
nc2 <- as.SpatialRings.map(ncmap, IDs)
plotSpatialRings(nc2)
invisible(text(getSRSringsLabptSlots(nc2), labels=getSRSringsIDSlots(nc2), cex=0.6))
\dontrun{
library(RArcInfo)
fl <- "http://www.census.gov/geo/cob/bdy/co/co90e00/co37_d90_e00.zip"
download.file(fl, "co37_d90_e00.zip")
e00 <- zip.file.extract("co37_d90.e00", "co37_d90_e00.zip")
e00toavc(e00, "nc")
arc <- get.arcdata(".", "nc")
pal <- get.paldata(".", "nc")
pat <- get.tabledata("info", "NC.PAT")
}
data(co37_d90_arc)
data(co37_d90_pal)
sapply(pal[[2]], function(x) length(x[[1]]))
data(co37_d90_pat)
IDs <- paste(pat$ST[-1], pat$CO[-1], sep="")
nc3 <- as.SpatialRings.pal(arc, pal, IDs=IDs)
plotSpatialRings(nc3)
invisible(text(getSRSringsLabptSlots(nc3), labels=getSRSringsIDSlots(nc3), cex=0.6))
}
\keyword{spatial}
