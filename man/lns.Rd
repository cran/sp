\name{lns}
\alias{lns}
\docType{data}
\title{Norwegian river center line data}
\description{
  Norwegian river center line data in shapefile format
}
\usage{data(lnsshp)}
\format{
  The format is as returned by \code{read.shape} of the maptools package
}

\source{
  The input data copyright Statens Kartverk 2001, and
were released (with a collection of other shapefiles) to the public domain
in July 2001. The link to the relevant page under \url{http://www.statkart.no}
has now expired, this copy downloaded 13 July 2001.
}

\examples{
\dontrun{
library(maptools)
lns <- read.shape(system.file("shapes/fylk-val.shp", package="maptools")[1])
}
data(lnsshp)
res <- shp2SLDF(lns)
plot(res)

}
\keyword{datasets}
