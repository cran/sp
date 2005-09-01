"point.in.polygon" = function(point.x, point.y, pol.x, pol.y) {
	.Call("R_point_in_polygon_sp", 
		as.numeric(point.x),
		as.numeric(point.y),
		as.numeric(pol.x),
		as.numeric(pol.y), 
		PACKAGE = "sp"
	)
}

pointsInPolygon = function(pts, Polygon) {
	pts = coordinates(pts)
	cc = getPolygonCoordsSlot(Polygon)
	point.in.polygon(pts[,1], pts[,2], cc[,1], cc[,2])
}

pointsInPolygons = function(pts, Polygons, which = FALSE) {
	rings = getPolygonsPolygonsSlot(Polygons)
	res = lapply(rings, function(x, pts) pointsInPolygon(pts, x), pts = pts)
	if (which) {
		ret = rep(as.numeric(NA), nrow(coordinates(pts)))
		for (i in seq(along = res))
			ret[res[[i]] > 0] = i
		
	} else {
		ret = rep(FALSE, nrow(coordinates(pts)))
		for (i in seq(along = res))
			ret = ret | (res[[i]] > 0)
	}
	ret
}

pointsInSpatialPolygons = function(pts, SpRings) {
	sr = getSpPpolygonsSlot(SpRings)
	res = lapply(sr, function(x, pts) pointsInPolygons(pts, x), pts = pts)
	ret = rep(as.numeric(NA), nrow(coordinates(pts)))
	for (i in seq(along = res))
		ret[res[[i]] > 0] = i
	ret
}
