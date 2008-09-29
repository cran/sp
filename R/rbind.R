checkCRSequal = function(dots) {
	if (length(dots) > 1) {
		p1 = proj4string(dots[[1]])
		res = unlist(lapply(dots[-1], function(x) identical(proj4string(x), p1)))
		if (any(!res))
		#if (!isTRUE(all.equal(proj4string(obj), proj4string(x))))
			stop("coordinate reference systems differ")
	}
}

makeUniqueIDs <- function(lst) {
	ids = sapply(lst, function(i) slot(i, "ID"))
	if (any(duplicated(ids))) {
		ids <- make.unique(as.character(unlist(ids)), sep = "")
		for (i in seq(along = ids))
			lst[[i]]@ID = ids[i]
	}
	lst
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

rbind.SpatialPolygons = function(..., makeUniqueIDs = FALSE) {
	dots = list(...)
	checkCRSequal(dots)
	# checkIDSclash(dots)
	pl = do.call("c", lapply(dots, function(x) slot(x, "polygons")))
	if (makeUniqueIDs)
		pl = makeUniqueIDs(pl)
	SpatialPolygons(pl, proj4string = CRS(proj4string(dots[[1]])))
}

rbind.SpatialPolygonsDataFrame <- function(...) {
	dots = list(...)
	pl = do.call("rbind", lapply(dots, function(x) as(x, "SpatialPolygons")))
	df = do.call("rbind", lapply(dots, function(x) x@data))
	SpatialPolygonsDataFrame(pl, df)
}


rbind.SpatialLines = function(..., makeUniqueIDs = FALSE) {
	dots = list(...)
	checkCRSequal(dots)
	ll = do.call("c", lapply(dots, function(x) slot(x, "lines")))
	if (makeUniqueIDs)
		ll = makeUniqueIDs(ll)
	SpatialLines(ll, proj4string = CRS(proj4string(dots[[1]])))
}

rbind.SpatialLinesDataFrame <- function(...) {
	dots = list(...)
	ll = do.call("rbind", lapply(dots, function(x) as(x, "SpatialLines")))
	df = do.call("rbind", lapply(dots, function(x) x@data))
	SpatialLinesDataFrame(ll, df)
}
