spDistsN1 <- function(pts, pt, longlat=FALSE) {
	if (!is.matrix(pts)) stop("pts must be a matrix")
	if (ncol(pts) != 2) stop("pts must have two columns")
	if (!is.numeric(pts)) stop("pts must be numeric")
	if (!is.numeric(pt)) stop("pt must be numeric")
	if (length(pt) != 2) stop("pt must have length two")
	x <- as.double(pts[,1])
	y <- as.double(pts[,2])
	xx <- as.double(pt[1])
	yy <- as.double(pt[2])
	n  <- as.integer(length(x))
	dists <- vector(mode="double", length=n)
	lonlat <- as.integer(longlat)
	res <- .C("sp_dists", x, y, xx, yy, n, dists, lonlat, 
		PACKAGE = "sp")[[6]]
	if (any(!is.finite(res))) {
		nAn <- which(!is.finite(res))
		dx <- abs(x[nAn] - xx)
		dy <- abs(y[nAn] - yy)
		if (all((c(dx, dy) < .Machine$double.eps ^ 0.5)))
			res[nAn] <- 0
		else
			stop(paste("non-finite distances in spDistsN1"))

	}
	res
}

