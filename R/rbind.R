checkCRSequal = function(dots) {
	if (length(dots) > 1) {
		p1 = proj4string(dots[[1]])
		res = unlist(lapply(dots[-1], function(x) all.equal(proj4string(x), p1)))
		if (any(!res))
		#if (!isTRUE(all.equal(proj4string(obj), proj4string(x))))
			stop("coordinate reference systems differ")
	}
}

rbind.SpatialPoints <- function(...) {
	dots = list(...)
	checkCRSequal(dots)
	SpatialPoints(do.call("rbind", lapply(list(...), coordinates)), CRS(proj4string(dots[[1]])))
}

rbind.SpatialPointsDataFrame <- function(...) {
	dots = list(...)
	sp = do.call("rbind", lapply(dots, function(x) as(x, "SpatialPoints")))
	df = do.call("rbind", lapply(dots, function(x) x@data))
	SpatialPointsDataFrame(sp, df, coords.nrs = dots[[1]]@coords.nrs)
}

rbind.SpatialPixels = function(...) {
	dots = list(...)
	sp = do.call("rbind", lapply(dots, function(x) as(x, "SpatialPoints")))
	gridded(sp) = T
	sp
}

rbind.SpatialPixelsDataFrame = function(...) {
	dots = list(...)
	sp = do.call("rbind", lapply(dots, function(x) as(x, "SpatialPointsDataFrame")))
	gridded(sp) = T
	sp
}

rbind.SpatialPolygons = function(...) {
	dots = list(...)
	checkCRSequal(dots)
	# checkIDSclash(dots)
	pl = do.call("c", lapply(dots, function(x) slot(x, "polygons")))
	SpatialPolygons(pl, proj4string = CRS(proj4string(dots[[1]])))
}

rbind.SpatialPolygonsDataFrame <- function(...) {
	dots = list(...)
	pl = do.call("rbind", lapply(dots, function(x) as(x, "SpatialPolygons")))
	df = do.call("rbind", lapply(dots, function(x) x@data))
	SpatialPolygonsDataFrame(pl, df)
}

rbind.SpatialLines = function(...) {
	dots = list(...)
	checkCRSequal(dots)
	# checkIDSclash(dots)
	pl = do.call("c", lapply(dots, function(x) slot(x, "lines")))
	SpatialLines(pl, proj4string = CRS(proj4string(dots[[1]])))
}

rbind.SpatialLinesDataFrame <- function(...) {
	dots = list(...)
	pl = do.call("rbind", lapply(dots, function(x) as(x, "SpatialLines")))
	df = do.call("rbind", lapply(dots, function(x) x@data))
	SpatialLinesDataFrame(pl, df)
}
