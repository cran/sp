overlayPointsWithRings = function(x, y, fn = NULL) {
	# x = pts, y = rings, return ring with f(grid) items
	y = as(y, "SpatialRings")
	id = pointsInSpatialRings(x, y)
	if (!is.null(fn)) {
		df = as(x, "data.frame")
		data.frame(t(data.frame(lapply(split(df, id), fn))))
	} else
		id
}

setMethod("overlay", 
	signature(x = "SpatialPointsDataFrame", y = "SpatialRings"), 
		 overlayPointsWithRings)

setMethod("overlay", 
	signature(x = "SpatialPoints", y = "SpatialRings"), 
	function(x, y, ...) overlayPointsWithRings(x, y))  # no fn argument!

overlayRingsWithPoints = function(x, y, ...) {
	# x = rings, y = pts; return pts with ring values (or id) at grid point
	ypts = as(y, "SpatialPoints")
	sr = as(x, "SpatialRings")
	id = pointsInSpatialRings(ypts, sr)
	if (is(x, "SpatialRingsDataFrame")) {
		ret = x@data[id, , drop = FALSE]
		if (is(y, "SpatialPointsDataFrame"))
			row.names(ret) = row.names(as(y, "data.frame"))
		return(ret)
	} else
		return(id) # 
}

setMethod("overlay", signature("SpatialRings", "SpatialPoints"), 
	overlayRingsWithPoints)

overlayGridWithPoints = function(x, y, fn = NULL) {
	cc = coordinates(y)
	idx = getGridIndex(cc, x@grid, all.inside = FALSE)
	if (!fullgrid(x))
		idx = match(idx, x@grid.index)
	if (is(x, "SpatialGridDataFrame"))
		SpatialPointsDataFrame(cc, x@data[idx, ], proj4string = CRS(proj4string(x)))
	else
		return(idx)
}
setMethod("overlay", signature("SpatialGridDataFrame", "SpatialPoints"), 
	overlayGridWithPoints)

setMethod("overlay", signature("SpatialGrid", "SpatialPoints"), 
	overlayGridWithPoints)

#overlayPointsWithGrid = function(x, y, fn = NULL) {
#	cc = coordinates(x)
#	idx = getGridIndex(cc, y@grid, all.inside = FALSE)
#	if (!fullgrid(x))
#		idx = match(idx, x@grid.index)
#	if (is(x, "SpatialGridDataFrame"))
#		SpatialPointsDataFrame(cc, x@data[idx, ], proj4string = CRS(proj4string(x)))
#	else
#		return(idx)
#}
#
#setMethod("overlay", signature("SpatialPointsDataFrame", "SpatialGrid"), 
#	overlayPointsWithGrid)
#setMethod("overlay", signature("SpatialPoints", "SpatialGrid"), 
#	overlayPointsWithGrid)
