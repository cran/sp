\name{zerodist}
\alias{zerodist}
\alias{remove.duplicates}
\title{ find point pairs with equal spatial coordinates }
\description{ find point pairs with equal spatial coordinates }
\usage{ 
zerodist(obj, zero = 0.0) 
remove.duplicates(obj, zero = 0.0)
}
\arguments{
\item{obj}{ object of, or extending, class \link{SpatialPoints} }
\item{zero}{ value to be compared to for establishing when a distance
is considered zero (default 0.0) }
}
\value{pairs of row numbers with identical coordinates, numeric(0) if no
such pairs are found }
\note{ When using kriging, duplicate observations sharing identical spatial 
locations result in singular covariance matrices in kriging situations.
This function may help identifying spatial duplications, so they
can be removed.  A matrix with all pair-wise distances is calculated,
so if x, y and z are large this function is slow 
}
\examples{
data(meuse)
summary(meuse)
# pick 10 rows
n <- 10
ran10 <- sample(nrow(meuse), size = n, replace = TRUE)
meusedup <- rbind(meuse, meuse[ran10, ])
coordinates(meusedup) <- c("x", "y")
zd <- zerodist(meusedup)
sum(abs(zd[1:n,1] - sort(ran10))) # 0!
# remove the duplicate rows:
meusedup2 <- meusedup[-zd[,2], ]
summary(meusedup2)
meusedup3 <- subset(meusedup, !(1:nrow(meusedup) \%in\% zd[,2]))
summary(meusedup3)
}
\keyword{dplot}
