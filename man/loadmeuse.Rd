\name{loadMeuse}
\alias{loadMeuse}
\title{ load the Meuse data set }
\description{ load the Meuse data set }
\usage{
loadMeuse(gridded = TRUE)
}
\arguments{
\item{gridded}{ logical; if FALSE, meuse.grid will be imported as
SpatialPointsDataFrame, if TRUE as SpatialPixelsDataFrame }
}

\value{ none; it has a side effect that \link{meuse} and 
\link{meuse.grid} will be saved to the global environment }

\seealso{\link{meuse}, \link{meuse.grid}}
\examples{
library(sp)
loadMeuse()
summary(meuse)
summary(meuse.grid)
}
\keyword{dplot}
