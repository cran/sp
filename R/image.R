# first argument of image generic _needs_ to be x!
image.SpatialPixelsDataFrame = function(x, ...)
	image(as(x, "SpatialGridDataFrame"), ...)

image.SpatialGridDataFrame = function(x, attr = 1, xcol = 1, ycol = 2, 
		asp = 1, xlab, ylab, ...) {
	cnames = dimnames(coordinates(x))[[2]]
	if (missing(xlab))
		xlab = cnames[xcol]
	if (missing(ylab))
		ylab = cnames[ycol]
	image(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), 
		asp = asp, xlab = xlab, ylab = ylab, ...)
}

as.image.SpatialGridDataFrame = function(x, xcol = 1, ycol = 2) {
	cv = coordinatevalues(getGridTopology(x))
	m = as(x, "matrix")
	list(x = cv[[xcol]], y = sort(cv[[ycol]]), z = m[,ncol(m):1])
}
