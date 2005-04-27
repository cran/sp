\name{shp2SLDF}
\alias{shp2SLDF}
\alias{arcobj2SLDF}
\alias{contourLines2SLDF}
\alias{plotSpatialLines}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converter functions to build SpatialLinesDataFrame objects}
\description{
  These functions show how to build converters from external formats to build SpatialLinesDataFrame objects: \code{shp2SLDF} from \code{"arc"} type ESRI shapefiles read into \code{"Map"} objects using the \code{read.shape()} function in the maptools package; \code{arcobj2SLDF} from the list returned by the \code{get.arcdata} function in the RArcInfo package; and \code{contourLines2SLDF} from the list returned by the \code{contourLines} function in the graphics package (here the data frame is just the contour levels, with one Slines object made up of at least one Sline object per level). 
}
\usage{
shp2SLDF(shp, proj4string = CRS(as.character(NA)))
arcobj2SLDF(arc, proj4string=CRS(as.character(NA)))
contourLines2SLDF(cL, proj4string=CRS(as.character(NA)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shp}{a \code{"Map"} object returned by the \code{read.shape()} function in the maptools package}
  \item{arc}{a list returned by the \code{get.arcdata} function in the RArcInfo package}
  \item{cL}{a list returned by the \code{contourLines} function in the graphics package}
  \item{proj4string}{Object of class \code{"CRS"}; see \link{CRS-class}}
}

\value{
  A SpatialLinesDataFrame object
}

\author{ Roger Bivand; Edzer Pebesma }


\seealso{ \link{SpatialLines-class} }
\examples{
data(lnsshp) # retrieved as: 
# library(maptools)
# lns <- read.shape(system.file("shapes/fylk-val.shp", package="maptools")[1])
res <- shp2SLDF(lns)
plot(res)
invisible(title(""))
data(co37_d90_arc) # retrieved as: 
# library(RArcInfo)
# fl <- "http://www.census.gov/geo/cob/bdy/co/co90e00/co37_d90_e00.zip"
# download.file(fl, "co37_d90_e00.zip")
# e00 <- zip.file.extract("co37_d90.e00", "co37_d90_e00.zip")
# e00toavc(e00, "ncar")
# arc <- get.arcdata(".", "ncar")
res <- arcobj2SLDF(arc)
plot(res)
invisible(title(""))
res <- contourLines2SLDF(contourLines(volcano))
plot(res, col=terrain.colors(nrow(res@data)))
invisible(title(""))
}
\keyword{spatial}
