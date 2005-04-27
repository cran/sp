"point.in.polygon" = function(point.x, point.y, pol.x, pol.y) {
	.Call("R_point_in_polygon_sp", 
		as.numeric(point.x),
		as.numeric(point.y),
		as.numeric(pol.x),
		as.numeric(pol.y), 
		PACKAGE = "sp"
	)
}

pointsInSring = function(pts, Sring) {
	pts = coordinates(pts)
	cc = getSringCoordsSlot(Sring)
	point.in.polygon(pts[,1], pts[,2], cc[,1], cc[,2])
}

pointsInSrings = function(pts, Srings, which = FALSE) {
	rings = getSringsSringsSlot(Srings)
	res = lapply(rings, function(x, pts) pointsInSring(pts, x), pts = pts)
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

pointsInSpatialRings = function(pts, SpRings) {
	sr = getSRpolygonsSlot(SpRings)
	res = lapply(sr, function(x, pts) pointsInSrings(pts, x), pts = pts)
	ret = rep(as.numeric(NA), nrow(coordinates(pts)))
	for (i in seq(along = res))
		ret[res[[i]] > 0] = i
	ret
}
