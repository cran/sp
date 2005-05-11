makegrid = function(x, n = 10000, nsig = 2, cellsize, offset = c(0.5,0.5),
		type = "regular", ...) {
	bb = bbox(x)
	rx = bb[1,]
	ry = bb[2,]
	if (missing(cellsize))
		cellsize = signif(sqrt(diff(rx) * diff(ry)/n), nsig)
	minx = max(rx[1], signif(rx[1] + offset[1] * cellsize, nsig))
	miny = max(ry[1], signif(ry[1] + offset[2] * cellsize, nsig))
	seqx = seq(minx, rx[2], by = cellsize)
	seqy = seq(miny, ry[2], by = cellsize)
	# type = "regular" :
	xy = expand.grid(x = seqx, y = seqy)
	if (type == "stratified") {
		n = nrow(xy)
		xy$x = xy$x + (runif(n) - 0.5) * cellsize
		xy$y = xy$y + (runif(n) - 0.5) * cellsize
	} else if (type == "nonaligned") {
		nx = length(seqx)
		ny = length(seqy)
		x0 <- rep(runif(ny), rep(nx, ny))
		y0 <- rep(runif(nx), ny)
		xy$x = xy$x + (x0 - 0.5) * cellsize
		xy$y = xy$y + (y0 - 0.5) * cellsize
	} else if (type != "regular")
		stop(paste("sampling type", type, "not recognized"))
	return(xy)
}

sample.Spatial = function(x, n, type, bb = bbox(x), offset = runif(2), cellsize, ...) {
	if (type == "random") {
		xc = runif(n) * diff(bb[1,]) + bb[1,1]
		yc = runif(n) * diff(bb[2,]) + bb[2,1]
		xy = cbind(xc, yc)
	} else
		xy = makegrid(x, n = n, nsig = 20, cellsize = cellsize, 
			offset = offset, type = type)
	SpatialPoints(xy, CRS(proj4string(x)))
}
setMethod("spsample", signature(x = "Spatial"), sample.Spatial)

sample.Sline = function(x, n, type, offset = runif(1), ...) {
	cc = coordinates(x)
	dxy = apply(cc, 2, diff)
	lengths = apply(dxy, 1, function(x) sqrt(sum(x ** 2)))
	csl = c(0, cumsum(lengths))
	maxl = csl[length(csl)]
	if (type == "random")
		pts = runif(n) * maxl 
	else if (type == "stratified")
		pts = ((1:n) - runif(n))/n * maxl
	else if (type == "regular")
		pts = ((1:n) - offset)/n * maxl
	else
		stop(paste("type", type, "not available for Sline"))
	# find coordinates:
	int = findInterval(pts, csl, all.inside = TRUE)
	where = (pts - csl[int])/diff(csl)[int]
	xy = cc[int,] + where * (cc[int+1,] - cc[int,])
	SpatialPoints(xy, CRS(proj4string(x)))
}
setMethod("spsample", signature(x = "Sline"), sample.Sline)

sample.Sring = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), ...) {
	area = getSringAreaSlot(x)
	if (area == 0.0)
		spsample(Sline(getSringCoordsSlot(x), CRS(proj4string(x))), n, type, offset = offset[1])
	else {
		bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
		pts = spsample(as(x, "Spatial"), round(n * bb.area/area), type, offset = offset, ...)
		id = overlay(pts, SpatialRings(list(Srings(list(x),"xx"))))
		pts[which(!is.na(id))]
	}
}
setMethod("spsample", signature(x = "Sring"), sample.Sring)

sample.Srings = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), ...) {
	#stop("not functioning yet...")
	area = getSringAreaSlot(x) # also available for Srings!
	if (area == 0.0)
		# distribute n over the lines, according to their length?
		stop("sampling over multiple lines not functioning yet...")
	bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	pts = spsample(as(x, "Spatial"), round(n * bb.area/area), type, offset = offset, ...)
	id = overlay(pts, SpatialRings(list(x)))
	pts[which(!is.na(id))]
}
setMethod("spsample", signature(x = "Srings"), sample.Srings)

sample.SpatialRings = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), ...) {
	#stop("not functioning yet...")
	area = sum(unlist(lapply(getSRpolygonsSlot(x),getSringAreaSlot)))
	if (area == 0.0)
		stop("sampling over multiple lines not functioning yet...")
		# distribute n over the lines, according to their length?
	bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	pts = spsample(as(x, "Spatial"), round(n * bb.area/area), type, offset = offset, ...)
	pts[which(!is.na(overlay(pts, x)))]
}
setMethod("spsample", signature(x = "SpatialRings"), sample.SpatialRings)

sample.Sgrid = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), ...) {
	area = areaSpatialGrid(x)
	if (area == 0.0)
		stop("cannot sample from grid with zero area")
	bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	pts = spsample(as(x, "Spatial"), round(n * bb.area/area), type, offset = offset, ...)
	#id = overlay(as(x, "SpatialGrid"), pts)
	id = overlay(x, pts)
	if (is(id, "SpatialPointsDataFrame"))
		id = id@data[,1]
	pts[which(!is.na(id))]
}
setMethod("spsample", signature(x = "SpatialGrid"), sample.Sgrid)

sample.Spixels = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), ...) {
	area = areaSpatialGrid(x)
	if (area == 0.0)
		stop("cannot sample from grid with zero area")
	bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	pts = spsample(as(x, "Spatial"), round(n * bb.area/area), type, offset = offset, ...)
	#id = overlay(as(x, "SpatialGrid"), pts)
	id = overlay(x, pts)
	if (is(id, "SpatialPointsDataFrame"))
		id = id@data[,1]
	pts[which(!is.na(id))]
}
setMethod("spsample", signature(x = "SpatialPixels"), sample.Spixels)
