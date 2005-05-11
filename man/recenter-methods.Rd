\name{recenter-methods}
\docType{methods}
\alias{recenter-methods}
\alias{recenter}
\alias{recenter,SpatialRings-method}
\alias{recenter,Srings-method}
\alias{recenter,Sring-method}
\alias{recenter,SpatialLines-method}
\alias{recenter,Slines-method}
\alias{recenter,Sline-method}
\title{Methods for Function recenter in Package `sp'}
\description{
 Methods for function \code{recenter} in package \pkg{sp} to shift or re-center geographical coordinates for a Pacific view. All longitudes < 0 are added to 360, to avoid for instance parts of Alaska being represented on the far left and right of a plot because they have values straddling 180 degrees. In general, using a projected coordinate reference system is to be prefered, but this method permits a geographical coordinate reference system to be used. This idea was suggested by Greg Snow, and corresponds to the two world representations in the \pkg{maps} package. 
}
\section{Methods}{
\describe{

\item{obj = "SpatialRings"}{recenter a SpatialRings object}

\item{obj = "Srings"}{recenter a Srings object }

\item{obj = "Sring"}{recenter an Sring object }

\item{obj = "SpatialLines"}{recenter a SpatialLines object }

\item{obj = "Slines"}{recenter a Slines object }

\item{obj = "Sline"}{recenter an Sline object }
}}

\examples{
crds <- matrix(c(179, -179, -179, 179, 50, 50, 52, 52), ncol=2)
SL <- SpatialLines(list(Slines(list(Sline(crds, CRS("+proj=latlong"))))))
bbox(SL)
SLr <- recenter(SL)
bbox(SLr)
rcrds <- rbind(crds, crds[1,])
SR <- SpatialRings(list(Srings(list(Sring(rcrds, CRS("+proj=latlong"))), ID="r1")))
bbox(SR)
SRr <- recenter(SR)
bbox(SRr)
opar <- par(mfrow=c(1,2))
plot(SR)
plot(SRr)
par(opar)
crds <- matrix(c(-1, 1, 1, -1, 50, 50, 52, 52), ncol=2)
SL <- SpatialLines(list(Slines(list(Sline(crds, CRS("+proj=latlong"))))))
bbox(SL)
SLr <- recenter(SL)
bbox(SLr)
rcrds <- rbind(crds, crds[1,])
SR <- SpatialRings(list(Srings(list(Sring(rcrds, CRS("+proj=latlong"))), ID="r1")))
bbox(SR)
SRr <- recenter(SR)
bbox(SRr)
opar <- par(mfrow=c(1,2))
plot(SR)
plot(SRr)
par(opar)
}

\keyword{methods}

