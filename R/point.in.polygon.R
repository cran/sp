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
	cc = slot(Polygon, "coords")
	point.in.polygon(pts[,1], pts[,2], cc[,1], cc[,2])
}

pointsInPolygons = function(pts, Polygons, which = FALSE) {
	rings = slot(Polygons, "Polygons")
	res = matrix(unlist(lapply(rings, function(x, pts) 
		pointsInPolygon(pts, x), pts = pts)), ncol=length(rings))
	res <- res > 0
	holes <- sapply(rings, function(y) slot(y, "hole"))
	areas <- sapply(rings, function(x) slot(x, "area"))
	if (any(holes) && any(res[,holes])) {
		holerows <- which(res[,holes,drop=FALSE], arr.ind=TRUE)[,1]
		odd <- rowSums(res[holerows,,drop=FALSE])%%2 != 0
		for (i in seq(along = holerows)) {
			in_p <- which.min(areas[res[holerows[i],,drop=FALSE]])
			res[holerows[i],] <- FALSE
			if (odd[i]) res[holerows[i], in_p] <- TRUE
		}
		res[,holes] <- FALSE
	}
	ret <- apply(res, 1, any)
	if (which) {
		reta <- integer(length(ret))
		for (i in seq(along = ret)) {
			if (ret[i]) reta[i] <- which(res[i,])
			else reta[i] <- as.integer(NA)
		}
		ret <- reta
	}
	ret
}

pointsInSpatialPolygons = function(pts, SpPolygons) {
	sr = slot(SpPolygons, "polygons")
	res = lapply(sr, function(x, pts) pointsInPolygons(pts, x), pts = pts)
	ret = rep(as.numeric(NA), nrow(coordinates(pts)))
	for (i in seq(along = res))
		ret[res[[i]] > 0] = i
	ret
}
