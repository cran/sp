# first argument of image generic _needs_ to be x!
image.SpatialPixelsDataFrame = function(x, ...)
	image(as(x, "SpatialGridDataFrame"), ...)

image.SpatialGridDataFrame = function(x, attr = 1, xcol = 1, ycol = 2, 
		red=NULL, green=NULL, blue=NULL, asp = 1, axes = FALSE, xlim = NULL, 
		ylim = NULL, add = FALSE, ...) {

	if (!add)
		plot(as(x, "Spatial"), asp = asp, xlim = xlim, ylim = ylim, axes = axes)
	if (is.null(red)) 
		image(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), add = TRUE, asp = asp, ...)
	else {
		if (is.null(green) || is.null(blue)) 
			stop("all colour bands must be given")
		fcols <- factor(rgb(red=x@data[red][[1]], 
			green=x@data[green][[1]], 
			blue=x@data[blue][[1]], max=255))
		cv <- coordinatevalues(getGridTopology(x))
		m <- matrix(as.integer(fcols), x@grid@cells.dim[1], 
			x@grid@cells.dim[2], byrow=FALSE)
		res <- list(x=cv[[xcol]], y=sort(cv[[ycol]]), z=m[,ncol(m):1])
		image(res, col=levels(fcols), add = TRUE, asp = asp, ...)
	}
}

contour.SpatialGridDataFrame = function(x, ...) 
	contour(as.image.SpatialGridDataFrame(x), ...)

contour.SpatialPixelsDataFrame = function(x, ...)
	contour(as.image.SpatialGridDataFrame(as(x, "SpatialGridDataFrame")), ...)

as.image.SpatialGridDataFrame = function(x, xcol = 1, ycol = 2) {
	cv = coordinatevalues(getGridTopology(x))
	m = as(x, "matrix")
	list(x = cv[[xcol]], y = sort(cv[[ycol]]), z = m[,ncol(m):1])
}
