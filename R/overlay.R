overlayPointsWithRings = function(x, y, fn = NULL) {
	# x = pts, y = rings, return ring with f(grid) items
	y = as(y, "SpatialPolygons")
	id = pointsInSpatialPolygons(x, y)
	if (!is.null(fn)) {
		df = as(x, "data.frame")
		data.frame(t(data.frame(lapply(split(df, id), fn))))
	} else
		id
}

setMethod("overlay", 
	signature(x = "SpatialPointsDataFrame", y = "SpatialPolygons"), 
		 overlayPointsWithRings)

setMethod("overlay", 
	signature(x = "SpatialPoints", y = "SpatialPolygons"), 
	function(x, y, ...) overlayPointsWithRings(x, y))  # no fn argument!

overlayRingsWithPoints = function(x, y, ...) {
	# x = rings, y = pts; return pts with ring values (or id) at grid point
	ypts = as(y, "SpatialPoints")
	sr = as(x, "SpatialPolygons")
	id = pointsInSpatialPolygons(ypts, sr)
	if (is(x, "SpatialPolygonsDataFrame")) {
		ret = x@data[id, , drop = FALSE]
		if (is(y, "SpatialPointsDataFrame"))
			row.names(ret) = row.names(as(y, "data.frame"))
		return(ret)
	} else
		return(id) # 
}

setMethod("overlay", signature("SpatialPolygons", "SpatialPoints"), 
	overlayRingsWithPoints)

#overlayGridWithPoints = function(x, y, fn = NULL) {
#   cc = coordinates(y)
#   idx = getGridIndex(cc, x@grid, all.inside = FALSE)
#   if (!fullgrid(x))
#       idx = match(idx, x@grid.index)
#   if (is(x, "SpatialGridDataFrame")) {
#       data = as.data.frame(x@data[idx, ])  ## make sure we return a data frame
#       names(data) = names(x@data)
#       SpatialPointsDataFrame(cc, data, proj4string = CRS(proj4string(x)))
#       }
#   else
#       return(idx)
#}

overlayGridWithPoints = function(x, y, fn = NULL) {
	cc = coordinates(y)
	idx = getGridIndex(cc, x@grid, all.inside = FALSE)
	if (!fullgrid(x))
		idx = match(idx, x@grid.index)
	if (is(x, "SpatialGridDataFrame"))
		SpatialPointsDataFrame(cc, x@data[idx, , drop=FALSE], 
			proj4string = CRS(proj4string(x)))
	else
		return(idx)
}
setMethod("overlay", signature("SpatialGridDataFrame", "SpatialPoints"), 
	overlayGridWithPoints)

setMethod("overlay", signature("SpatialGrid", "SpatialPoints"), 
	overlayGridWithPoints)

setMethod("overlay", signature("SpatialPixelsDataFrame", "SpatialPoints"), 
	overlayGridWithPoints)

setMethod("overlay", signature("SpatialPixels", "SpatialPoints"), 
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
