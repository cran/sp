"zerodist" <-
function(obj, zero = 0.0)
{
	if (!extends(class(obj), "SpatialPoints"))
		stop("obj should be of, or extend, class SpatialPoints")
	# calculates matrix with pairwise distances for 
	# coordinate vectors x and y:
	cc = coordinates(obj)
	D <- outer(cc[,1], cc[,1], "-")^2 
	diag(D) <- 1
	if (!any(D <= zero))
		return(numeric(0))
	D <- D + outer(cc[,2], cc[,2], "-")^2
	diag(D) <- 1
	if (!any(D <= zero))
		return(numeric(0))
	if (NCOL(cc) > 2)
		D <- D + outer(cc[,3], cc[,3], "-")^2
	diag(D) <- 1
	n <- NROW(cc)
	index <- 1:(n*n)
	z <- index[as.vector(D) <= zero]
	ret <- cbind(((z - 1)  %/% n) + 1, ifelse(z %% n == 0, n, z %% n))
	matrix(ret[ret[,1] < ret[,2],], ncol = 2)
}

remove.duplicates <- function(obj, zero = 0.0) {
	zd = zerodist(obj, zero)
	res <- subset(obj, !(1:nrow(obj) %in% zd[,2]))
	res
#	obj[-zd[,2], ]
}
