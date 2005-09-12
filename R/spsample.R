makegrid = function(x, n = 10000, nsig = 2, cellsize, offset = c(0.5,0.5),
		type = "regular", ...) {
#	bb = bbox(x)
	bb = x
	rx = bb[1,]
	ry = bb[2,]
	if (missing(cellsize))
		cellsize = signif(sqrt(diff(rx) * diff(ry)/n), nsig)
# in some cases with small n, min* can be larger than bbox max values
# so guard imposed to step down from cellsize
	minx = max(rx[1], signif(rx[1] + offset[1] * cellsize, nsig))
	miny = max(ry[1], signif(ry[1] + offset[2] * cellsize, nsig))
	if (minx < rx[2]) seqx = seq(minx, rx[2], by = cellsize)
	else seqx = seq(minx, rx[2], by = -cellsize)
	if (miny < ry[2]) seqy = seq(miny, ry[2], by = cellsize)
	else seqy = seq(miny, ry[2], by = -cellsize)
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
		xy = makegrid(#x
bbox(x), n = n, nsig = 20, cellsize = cellsize, 
			offset = offset, type = type)
	SpatialPoints(xy, CRS(proj4string(x)))
}
setMethod("spsample", signature(x = "Spatial"), sample.Spatial)

sample.Line = function(x, n, type, offset = runif(1),
proj4string=CRS(as.character(NA)), ...) {
#...) {
	cc = coordinates(x)
	dxy = apply(cc, 2, diff)
	if (inherits(dxy, "matrix"))
		lengths = apply(dxy, 1, function(x) sqrt(sum(x ** 2)))
	else # cc has 2 rows:
		lengths = sqrt(sum(dxy ** 2))
	csl = c(0, cumsum(lengths))
	maxl = csl[length(csl)]
	if (type == "random")
		pts = runif(n) * maxl 
	else if (type == "stratified")
		pts = ((1:n) - runif(n))/n * maxl
	else if (type == "regular")
		pts = ((1:n) - offset)/n * maxl
	else
		stop(paste("type", type, "not available for Line"))
	# find coordinates:
	int = findInterval(pts, csl, all.inside = TRUE)
	where = (pts - csl[int])/diff(csl)[int]
	xy = cc[int,] + where * (cc[int+1,] - cc[int,])
#	SpatialPoints(xy, CRS(proj4string(x)))
	SpatialPoints(xy, proj4string=proj4string)
}
setMethod("spsample", signature(x = "Line"), sample.Line)

sample.Polygon = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), proj4string=CRS(as.character(NA)), iter=4, ...) {
#...) {
	area = getPolygonAreaSlot(x)
	if (area == 0.0)
		spsample(Line(getPolygonCoordsSlot(x)), 
			n, type, offset = offset[1], proj4string=proj4string)
#CRS(proj4string(x))), n, type, offset = offset[1])
	else {
		res <- NULL
		its <- 0
		while (is.null(res) && its < iter) {
		    bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
		    xSP <- new("Spatial", bbox=bbox(x), proj4string=proj4string)
		    pts = sample.Spatial(
#spsample(
#as(x, "Spatial")
xSP, round(n * bb.area/area), type=type, offset = offset, ...)
# FIXME!!
		    id = overlay(pts, SpatialPolygons(list(Polygons(list(x),
			"xx")), proj4string=proj4string))
		    Not_NAs <- !is.na(id)
		    if (!any(Not_NAs)) res <- NULL
		    else res <- pts[which(Not_NAs)]
		    its <- its+1
		}
		res
	}
}
setMethod("spsample", signature(x = "Polygon"), sample.Polygon)

sample.Polygons = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), 
#...) {
proj4string=CRS(as.character(NA)), iter=4, ...) {
	#stop("not functioning yet...")
	area = getPolygonAreaSlot(x) # also available for Polygons!
	if (area == 0.0)
		# distribute n over the lines, according to their length?
		stop("sampling over multiple lines not functioning yet...")
	res <- NULL
	its <- 0
	while (is.null(res) && its < iter) {
	    bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	    xSP <- new("Spatial", bbox=bbox(x), proj4string=proj4string)
	    pts = sample.Spatial(
#spsample(
#as(x, "Spatial")
xSP, round(n * bb.area/area), type=type, offset = offset, ...)
# FIXME!!
	    id = overlay(pts, SpatialPolygons(list(x), proj4string=proj4string))
	    Not_NAs <- !is.na(id)
	    if (!any(Not_NAs)) res <- NULL
	    else res <- pts[which(Not_NAs)]
	    its <- its+1
	}
	res
}
setMethod("spsample", signature(x = "Polygons"), sample.Polygons)

sample.SpatialPolygons = function(x, n, type = "random", bb = bbox(x),
		offset = runif(2), iter=4, ...) {
	#stop("not functioning yet...")
	area = sum(unlist(lapply(getSpPpolygonsSlot(x),getPolygonAreaSlot)))
	if (area == 0.0)
		stop("sampling over multiple lines not functioning yet...")
		# distribute n over the lines, according to their length?
	res <- NULL
	its <- 0
	while (is.null(res) && its < iter) {
	    bb.area = prod(apply(bb, 1, function(x) diff(range(x))))
	    pts = #spsample(as(x, "Spatial")
sample.Spatial(as(x, "Spatial"), round(n * bb.area/area), type=type, offset = offset, ...)
	    Over_pts_x <- overlay(pts, x)
	    Not_NAs <- !is.na(Over_pts_x)
	    if (!any(Not_NAs)) res <- NULL
	    else res <- pts[which(Not_NAs)]
	    its <- its+1
	}
	res
}
setMethod("spsample", signature(x = "SpatialPolygons"), sample.SpatialPolygons)

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
		#id = id@data[,1]
		id = id@data@att[[1]]
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
		id = id@data@att[[1]]
	pts[which(!is.na(id))]
}
setMethod("spsample", signature(x = "SpatialPixels"), sample.Spixels)
