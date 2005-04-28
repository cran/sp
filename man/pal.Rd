\name{pal}
\alias{pal}
\alias{arc}
\alias{pat}
\docType{data}
\title{North Carolina county boundaries (e00)}
\description{
  North Carolina county boundaries in e00 (ArcInfo) format for counties from the 1990 census.
}
\usage{
data(co37_d90_arc)
data(co37_d90_pal)
data(co37_d90_pat)
}
\format{
  The format is as read by functions in the RArcInfo package

}

\source{
  \url{http://www.census.gov/geo/cob/bdy/co/co90e00/co37_d90_e00.zip}
}

\examples{
\dontrun{
library(RArcInfo)
fl <- "http://www.census.gov/geo/cob/bdy/co/co90e00/co37_d90_e00.zip"
download.file(fl, "co37_d90_e00.zip")
e00 <- zip.file.extract("co37_d90.e00", "co37_d90_e00.zip")
e00toavc(e00, "nc")
arc <- get.arcdata(".", "nc")
pal <- get.paldata(".", "nc")
sapply(pal[[2]], function(x) length(x[[1]]))
pat <- get.tabledata("info", "NC.PAT")
}
data(co37_d90_arc)
data(co37_d90_pal)
data(co37_d90_pat)
IDs <- paste(pat$ST[-1], pat$CO[-1], sep="")
nc3 <- as.SpatialRings.pal(arc, pal, IDs=IDs)
plotSpatialRings(nc3)
invisible(text(getSRSringsLabptSlots(nc3), labels=getSRSringsIDSlots(nc3), cex=0.6))
}
\keyword{datasets}
