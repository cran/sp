\name{ncmap}
\alias{ncmap}
\docType{data}
\title{North Carolina county boundaries (maps)}
\description{
  North Carolina county boundaries in maps package format
}
\usage{data(ncmap)}
\format{
  The format is:
List of 4
 $ x    : num [1:3771] -79.5 -79.5 -79.5 -79.5 -79.5 ...
 $ y    : num [1:3771] 35.8 35.9 35.9 36.0 36.2 ...
 $ range: num [1:4] -84.3 -75.5  33.9  36.6
 $ names: chr [1:102] "north carolina,alamance" "north carolina,alexander" ...
 - attr(*, "class")= chr "map"
}

\examples{
\dontrun{
library(maps)
ncmap <- map("county", "north carolina", fill=TRUE, col="transparent", plot=FALSE)
}
data(ncmap)
IDs <- sapply(strsplit(ncmap$names, ","), function(x) x[2])
nc2 <- as.SpatialRings.map(ncmap, IDs)
plotSpatialRings(nc2)
invisible(text(getSRSringsLabptSlots(nc2), labels=getSRSringsIDSlots(nc2), cex=0.6))
}
\keyword{datasets}
