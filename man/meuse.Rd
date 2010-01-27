\name{meuse}
\alias{meuse}
\docType{data}
\title{Meuse river data set}
\description{
This data set gives locations and top soil heavy metal concentrations
(ppm), along with a number of soil and landscape variables, collected in
a flood plain of the river Meuse, near the village Stein. Heavy metal
concentrations are bulk sampled from an area of approximately 15 m
x 15 m.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{x}{a numeric vector; x-coordinate (m)
	in RDM (Dutch topographical map coordinates) }
    \item{y}{a numeric vector; y-coordinate (m)
	in RDM (Dutch topographical map coordinates)}
	\item{cadmium}{topsoil cadmium concentration, ppm.; note that zero cadmium
	values in the original data set have been shifted to 0.2 (half the lowest 
	non-zero value) }
	\item{copper}{topsoil copper concentration, ppm. }
	\item{lead}{topsoil lead concentration, ppm. }
	\item{zinc}{topsoil zinc concentration, ppm. } 
	\item{elev}{relative elevation}
	\item{dist}{distance to river Meuse; obtained from the nearest cell in
	\link{meuse.grid}, which in turn was derived by a spread (spatial distance)
	GIS operation, therefore it is accurate up to 20 metres; 
	normalized $[0,1]$ }
	\item{om}{organic matter, as percentage }
	\item{ffreq}{flooding frequency class}
	\item{soil}{soil type}
	\item{lime}{lime class}
	\item{landuse}{landuse class}
	\item{dist.m}{distance to river Meuse (metres), as obtained during
	the field survey}
  }
}
\usage{
data(meuse)
}
\note{row.names refer to the original sample number}
\author{ The actual field data were collected by Ruud van Rijn and
Mathieu Rikken; compiled for R by Edzer Pebesma }
\references{ 
P.A. Burrough, R.A. McDonnell, 1998. Principles of Geographical Information
Systems. Oxford University Press.

\url{http:/www.gstat.org/}
}
\keyword{datasets}

\examples{
data(meuse)
summary(meuse)
}
