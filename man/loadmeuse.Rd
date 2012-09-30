\name{loadMeuse}
\alias{loadMeuse}
\title{ load the Meuse data set }
\description{ load the Meuse data set }
\usage{
loadMeuse(gridded = TRUE, river = FALSE)
}
\arguments{
\item{gridded}{ logical; if FALSE, meuse.grid will be imported as
SpatialPointsDataFrame, if TRUE as SpatialPixelsDataFrame }
\item{river}{ logical; load polygon for \link{meuse.riv}}
}

\value{ none; it has a side effect that \link{meuse},
\link{meuse.grid} and potentially \link{meuse.riv} will be saved to
(and possibly overwritten in) the global environment }

\seealso{\link{meuse}, \link{meuse.grid}}
\examples{
library(sp)
loadMeuse(river=TRUE)
summary(meuse)
summary(meuse.grid)
summary(meuse.riv)
}
\keyword{dplot}
