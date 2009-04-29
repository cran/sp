SpatialPixelsDataFrame = function(points, data, 
		tolerance = sqrt(.Machine$double.eps), 
		proj4string = CRS(as.character(NA)), round=NULL, fuzz.tol=3) {
	if (is.null(points))
		stop("points argument is NULL")
	if (!is(points, "SpatialPoints"))
		points = SpatialPoints(points, proj4string = proj4string)
	points = SpatialPixels(points, tolerance=tolerance, round=round, 
		fuzz.tol=fuzz.tol)
	new("SpatialPixelsDataFrame", points, data = data)
}

SpatialGridDataFrame = function(grid, data, proj4string = CRS(as.character(NA)))
	new("SpatialGridDataFrame", SpatialGrid(grid, proj4string), data = data)

as.SPixDF.SGDF = function(from) {
   	data = list()
   	n = .NumberOfCells(from@grid)
   	for (i in seq(along = from@data)) {
		v = vector(mode(from@data[[i]]), n)
      	if (is.factor(from@data[[i]]))
			v = factor(rep(NA, n), levels = levels(from@data[[i]]))
		else
			v[-from@grid.index] = NA
		v[from@grid.index] = from@data[[i]]
		data[[i]] = v
   	}
   	data = data.frame(data)
   	names(data) = names(from@data)
	SpatialGridDataFrame(from@grid, data, CRS(proj4string(from)))
}
setAs("SpatialPixelsDataFrame", "SpatialGridDataFrame", as.SPixDF.SGDF)

as.SGDF.SPixDF = function(from) { 
	# find rows with only NA's in attribute table:
	sel = apply(sapply(from@data, is.na), 1, function(x) !all(x))
	if (!any(sel)) {
		warning("complete map seems to be NA's -- no selection was made")
		sel = rep(TRUE, length(sel))
	}
   	#SpatialPixelsDataFrame(points = coordinates(from)[sel,], 
	#	data = from@data[sel,,drop=FALSE], proj4string = CRS(proj4string(from)))
	new("SpatialPixelsDataFrame", 
		new("SpatialPixels", 
			new("SpatialPoints", coords = coordinates(from)[sel,], 
				bbox = from@bbox, proj4string = CRS(proj4string(from))),
			grid = from@grid, 
			grid.index = which(sel)),
		data = from@data[sel,,drop=FALSE])
}
setAs("SpatialGridDataFrame", "SpatialPixelsDataFrame", as.SGDF.SPixDF)
setAs("SpatialGridDataFrame", "SpatialPointsDataFrame", 
	function(from) as(as(from, "SpatialPixelsDataFrame"), "SpatialPointsDataFrame"))

setMethod("coordinates", "SpatialPixelsDataFrame", 
	function(obj) coordinates(as(obj, "SpatialPixels")))

setMethod("coordinates", "SpatialGridDataFrame", 
	function(obj) coordinates(as(obj, "SpatialGrid")))

setIs("SpatialPixelsDataFrame", "SpatialPointsDataFrame",
	coerce = function(from) {
		# fullgrid(from) = FALSE ## not needed anymore
		new("SpatialPointsDataFrame",
			as(from, "SpatialPoints"), data = from@data, coords.nrs = from@coords.nrs)
	}, replace = function(obj, value) stop("no replace function for this coercion")
)

as.SpatialPolygonsDataFrame.SpatialPixelsDataFrame = function(from)
	SpatialPolygonsDataFrame(as(from, "SpatialPolygons"), from@data, FALSE)
setAs("SpatialPixelsDataFrame", "SpatialPolygonsDataFrame",
	as.SpatialPolygonsDataFrame.SpatialPixelsDataFrame)

as.matrix.SpatialPixelsDataFrame = function(x, ...) {
	# fullgrid(x) = TRUE
	x = as(x, "SpatialGridDataFrame")
	as(x, "matrix", ...)
}

as.matrix.SpatialGridDataFrame = function(x, ..., byrow = FALSE) {
	if (ncol(x@data) > 1)
		warning(
		"as.matrix.SpatialPixelsDataFrame uses first column;\n pass subset or [] for other columns")
	# try, at some stage also:
	# matrix(x@data[[1]], x@grid@cells.dim[2], x@grid@cells.dim[1], byrow=TRUE)
	matrix(x@data[[1]], x@grid@cells.dim[1], x@grid@cells.dim[2], byrow=byrow)
}

setAs("SpatialPixelsDataFrame", "matrix", function(from) 
	as.matrix.SpatialPixelsDataFrame(from))
setAs("SpatialGridDataFrame", "matrix", function(from) 
	as.matrix.SpatialGridDataFrame(from))

as.data.frame.SpatialPixelsDataFrame = function(x, row.names, optional, ...)
	as.data.frame(as(x, "SpatialPointsDataFrame"))

as.data.frame.SpatialGridDataFrame = function(x, row.names, optional, ...)
	as.data.frame(as(x, "SpatialPixelsDataFrame"))

setAs("SpatialPixelsDataFrame", "data.frame", function(from)
	as.data.frame.SpatialPixelsDataFrame(from))
setAs("SpatialGridDataFrame", "data.frame", function(from)
	as.data.frame.SpatialGridDataFrame(from))

subset.SpatialPixelsDataFrame <- function(x, subset, select, drop = FALSE, ...) {
	xSP <- coordinates(x)
	dfSP <- as.data.frame(x)
	cselect <- colnames(xSP)
	points <- subset(xSP, subset=subset, select=cselect, drop = drop, ...)
	if (missing(select)) select <- names(dfSP)
	data <- subset(dfSP, subset=subset, select=select, drop = drop, ...)
	SpatialPixelsDataFrame(points, data)
}

subs.SpatialPixelsDataFrame <- function(x, i, j, ... , drop = FALSE) {
	n.args = nargs()
	if (!missing(drop))
		stop("don't supply drop: it needs to be FALSE anyway")
	if (missing(i) && missing(j))
		return(x)
	if (missing(j)) {
		if (n.args == 3) # with a , : x[i,]
			res = as(x, "SpatialPointsDataFrame")[i = i, TRUE, ...]
		else { # withouth a , : x[i] --- column selection
			#res = as(x, "SpatialPointsDataFrame")[TRUE, j = i, ...]
			res = x
			res@data = res@data[TRUE, j = i, ..., drop = drop]
		}
	} else if (missing(i))
		res = as(x, "SpatialPointsDataFrame")[TRUE, j = j, ...]
	else
		res = as(x, "SpatialPointsDataFrame")[i = i, j = j, ...]
	gridded(res) = TRUE
	res
}
setMethod("[", "SpatialPixelsDataFrame", subs.SpatialPixelsDataFrame)

subs.SpatialGridDataFrame <- function(x, i, j, ... , drop = FALSE) {
	n.args = nargs()
	dots = list(...)
	if (!missing(drop))
		stop("don't supply drop: it needs to be FALSE anyway")
	missing.i = missing(i)
	missing.j = missing(j)
	if (length(dots) > 0) {
		missing.k = FALSE
		k = dots[[1]]
	} else
		missing.k = TRUE
	if (missing.i && missing.j && missing.k)
		return(x)
	grd = x@grid

	if (missing.k) {
		k = TRUE
		if (missing.j && n.args != 3) { # not like : x[i,]
			x@data = x@data[ , i, drop = FALSE]
			return(x)
		}
	} else if (missing.j && n.args == 2) {
		x@data = x@data[ , k, drop = FALSE]
		return(x)
	} 
	if (missing.i)
		rows = 1:grd@cells.dim[2]
	else
		rows = i
	if (missing.j)
		cols = 1:grd@cells.dim[1]
	else
		cols = j
	idx = 1:prod(grd@cells.dim[1:2])
	m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[rows,cols]
	idx = as.vector(m) # t(m)?
	if (any(is.na(idx)))
		stop("NAs not permitted in index")
	pts = SpatialPoints(coordinates(x)[idx,], CRS(proj4string(x)))
	res = SpatialPixelsDataFrame(SpatialPixels(pts), x@data[idx, k, drop = FALSE])
	as(res, "SpatialGridDataFrame")
}
setMethod("[", "SpatialGridDataFrame", subs.SpatialGridDataFrame)

cbind.SpatialGridDataFrame = function(...) { 
	stop.ifnot.equal = function(a, b) {
		res = all.equal(getGridTopology(a), getGridTopology(b))
		if (!is.logical(res) || !res)
			stop("topology is not equal")
	}
	grds = list(...)
	ngrds = length(grds)
	if (ngrds < 1)
		stop("no arguments supplied")
	if (ngrds == 1)
		return(grds[[1]])
	# verify matching topology:
	sapply(grds[2:ngrds], function(x) stop.ifnot.equal(x, grds[[1]]))
	gr = grds[[1]]
	gr@data = do.call("cbind", lapply(grds, function(x) x@data))
	#for (i in 2:ngrds)
	#	gr@data = cbind(gr@data, grds[[i]]@data)
	proj4string(gr) = CRS(proj4string(grds[[1]]))
	gr
}

print.SpatialPixelsDataFrame = function(x, ...) {
	cat("Object of class SpatialPixelsDataFrame\n")
	print(as(x, "SpatialPixels"))
	cat("\n")
	cat("Data summary:\n")
        if (ncol(x@data) > 1)
                sobj = summary(x@data)
            else sobj = summary(x@data[[1]])
	print(sobj)
	invisible(x)
}
setMethod("show", "SpatialPixelsDataFrame", 
	function(object) print.SpatialPixelsDataFrame(object))

print.SpatialGridDataFrame = function(x, ...) {
	cat("Object of class SpatialGridDataFrame\n")
	print(as(x, "SpatialGrid"))
	cat("\n")
	cat("Data summary:\n")
        if (ncol(x@data) > 1)
                sobj = summary(x@data)
            else sobj = summary(x@data[[1]])
	print(sobj)
	invisible(x)
}
setMethod("show", "SpatialGridDataFrame", 
	function(object) print.SpatialGridDataFrame(object))

names.SpatialPixelsDataFrame = function(x) names(x@data)
names.SpatialGridDataFrame = function(x) names(x@data)

"names<-.SpatialPixelsDataFrame" = function(x,value) { names(x@data) = value; x }
"names<-.SpatialGridDataFrame" = function(x,value) { names(x@data) = value; x }

dim.SpatialPixelsDataFrame = function(x) dim(x@data)


dim.SpatialGridDataFrame = function(x) dim(x@data)


