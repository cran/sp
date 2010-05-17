#"zerodist" <-
#function(obj, zero = 0.0)
#{
#	if (!extends(class(obj), "SpatialPoints"))
#		stop("obj should be of, or extend, class SpatialPoints")
#	# calculates matrix with pairwise distances for 
#	# coordinate vectors x and y:
#	cc = coordinates(obj)
#	D <- outer(cc[,1], cc[,1], "-")^2 
#	diag(D) <- 1
#	if (!any(D <= zero))
#		return(numeric(0))
#	D <- D + outer(cc[,2], cc[,2], "-")^2
#	diag(D) <- 1
#	if (!any(D <= zero))
#		return(numeric(0))
#	if (NCOL(cc) > 2)
#		D <- D + outer(cc[,3], cc[,3], "-")^2
#	diag(D) <- 1
#	n <- NROW(cc)
#	index <- 1:(n*n)
#	z <- index[as.vector(D) <= zero]
#	ret <- cbind(((z - 1)  %/% n) + 1, ifelse(z %% n == 0, n, z %% n))
#	matrix(ret[ret[,1] < ret[,2],], ncol = 2)
#}

zerodist <- function(obj, zero = 0.0, unique.ID = FALSE) {
	if (!extends(class(obj), "SpatialPoints"))
		stop("obj should be of, or extend, class SpatialPoints")
	# calculates matrix with pairwise distances for 
	# coordinate vectors x and y:
	cc = coordinates(obj)
	zd = matrix(.Call("sp_zerodist", as.vector(t(cc)), ncol(cc), zero), 
		ncol = 2, byrow = TRUE) + 1
	if (unique.ID) {
		id = 1:nrow(cc)
		id[zd[,1]] = id[zd[,2]]
		return(id)
	} else
		return(zd)
}

zerodist2 <- function (obj1, obj2, zero = 0) {
    if (!(extends(class(obj1), "SpatialPoints")
    		&& extends(class(obj2), "SpatialPoints"))) 
        stop("obj1 and obj2 should be of, or extend, class SpatialPoints")
    cc1 = coordinates(obj1)
    cc2 = coordinates(obj2)
	n = nrow(cc1)
	cc = rbind(cc1, cc2)
	ret = matrix(.Call("sp_zerodist", as.vector(t(cc)), ncol(cc), zero), 
		ncol = 2, byrow = TRUE) + 1
	ret = ret[ret[,1] <= n & ret[,2] > n,]
	ret[,2] = ret[,2] - n
	ret
}

remove.duplicates <- function(obj, zero = 0.0, remove.second = TRUE) {
	zd = zerodist(obj, zero)
	if (nrow(zd) > 0) {
		if (remove.second) 
			idx = 2
		else
			idx = 1
		obj[-zd[,idx], ]
	} else
		obj
}
