# first argument of image generic _needs_ to be x!
image.SpatialPixelsDataFrame = function(x, ...)
	image(as(x, "SpatialGridDataFrame"), ...)

image.SpatialGridDataFrame = function(x, attr = 1, xcol = 1, ycol = 2, 
		red=NULL, green=NULL, blue=NULL, axes = FALSE, xlim = NULL, 
		ylim = NULL, add = FALSE, ..., asp = NA, setParUsrBB=FALSE) {

	if (!add)
		plot(as(x, "Spatial"),
			xlim = xlim, ylim = ylim, axes = axes, asp = asp, ..., 
			setParUsrBB=setParUsrBB)
	if (is.null(red)) 
		image(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), add = TRUE, ...)
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
		image(res, col=levels(fcols), add = TRUE, ...)
	}
}

contour.SpatialGridDataFrame = function(x, attr = 1, xcol = 1, ycol = 2, 
		add = FALSE, xlim = NULL, ylim = NULL, axes = FALSE, ..., setParUsrBB = FALSE)  {
	if (!add)
		plot(as(x, "Spatial"),
			xlim = xlim, ylim = ylim, axes = axes, ..., 
			setParUsrBB=setParUsrBB)
	contour(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), add = TRUE, ...)
}

contour.SpatialPixelsDataFrame = function(x, ...)
	contour(as(x, "SpatialGridDataFrame"), ...)

as.image.SpatialGridDataFrame = function(x, xcol = 1, ycol = 2) {
	cv = coordinatevalues(getGridTopology(x))
	m = as(x, "matrix")
	list(x = cv[[xcol]], y = sort(cv[[ycol]]), z = m[,ncol(m):1])
}
