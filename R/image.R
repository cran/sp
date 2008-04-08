# first argument of image generic _needs_ to be x!
image.SpatialPixelsDataFrame = function(x, ...)
	image(as(x, "SpatialGridDataFrame"), ...)

image.SpatialGridDataFrame = function(x, attr = 1, xcol = 1, ycol = 2,
                col = heat.colors(12), 
		red=NULL, green=NULL, blue=NULL, axes = FALSE, xlim = NULL, 
		ylim = NULL, add = FALSE, ..., asp = NA, 
		setParUsrBB=FALSE) {

	if (!add)
		plot(as(x, "Spatial"),
			xlim = xlim, ylim = ylim, axes = axes, asp = asp, ..., 
			setParUsrBB=setParUsrBB)
	if (is.null(red)) 
		image(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), 
                  add = TRUE, col = col, ...)
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
                col = 1, add = FALSE, xlim = NULL, ylim = NULL,
                axes = FALSE, ..., setParUsrBB = FALSE)  {
	if (!add)
		plot(as(x, "Spatial"),
			xlim = xlim, ylim = ylim, axes = axes, ..., 
			setParUsrBB=setParUsrBB)
	contour(as.image.SpatialGridDataFrame(x[attr], xcol, ycol), col = col,
          add = TRUE, ...)
}

contour.SpatialPixelsDataFrame = function(x, ...)
	contour(as(x, "SpatialGridDataFrame"), ...)

as.image.SpatialGridDataFrame = function(x, xcol = 1, ycol = 2) {
	cv = coordinatevalues(getGridTopology(x))
	m = as(x, "matrix")
	list(x = cv[[xcol]], y = sort(cv[[ycol]]), z = m[,ncol(m):1])
}

# contributed by Michael Sumner 24 Oct 2007

image2Grid <- function (im, p4 = as.character(NA)) 
{
    if (!all(c("x", "y", "z") %in% names(im))) 
        stop("image must have components x, y, and z")
# RSB reversed test order
    cells.dim <- dim(im$z)
    xx <- im$x
    yy <- im$y
    lx <- length(xx)
    ly <- length(yy)
    if (all(c(lx, ly) == (cells.dim + 1))) {
        print("corners")
        xx <- xx[-1] - diff(xx[1:2])/2
        yy <- yy[-1] - diff(yy[1:2])/2
    }
    SpatialGridDataFrame(GridTopology(c(xx[1], yy[1]), c(diff(xx[1:2]), 
        diff(yy[1:2])), cells.dim), data.frame(z = as.vector(im$z[, 
        ncol(im$z):1])), proj4string = CRS(p4))
}


