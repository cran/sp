\name{zerodist}
\alias{zerodist}
\alias{zerodist2}
\alias{remove.duplicates}
\title{ find point pairs with equal spatial coordinates }
\description{ find point pairs with equal spatial coordinates }
\usage{ 
zerodist(obj, zero = 0.0) 
zerodist2(obj1, obj2, zero = 0.0) 
remove.duplicates(obj, zero = 0.0, remove.second = TRUE)
}
\arguments{
\item{obj}{ object of, or extending, class \link{SpatialPoints} }
\item{obj1}{ object of, or extending, class \link{SpatialPoints} }
\item{obj2}{ object of, or extending, class \link{SpatialPoints} }
\item{zero}{ distance values less than or equal to this threshold value are
considered to have zero distance (default 0.0) }
\item{remove.second}{logical; if TRUE, the second of each pair of duplicate
points is removed, if not the first}
}
\value{pairs of row numbers with identical coordinates; matrix with zero rows 
if no such pairs are found. For \code{zerodist}, row number pairs refer to row
pairs in \code{obj}. For \code{zerodist2}, row number pairs refer to rows in \code{obj}
and \code{obj2}, respectively. }
\note{ When using kriging, duplicate observations sharing identical spatial 
locations result in singular covariance matrices in kriging situations.
This function may help identifying spatial duplications, so they
can be removed.  The full matrix with all pair-wise distances is not stored;
the double loop is done at the C level.
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
coordinates(meuse) <- c("x", "y")
zerodist2(meuse, meuse[c(10:33,1,10),])
}
\keyword{dplot}
