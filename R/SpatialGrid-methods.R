SpatialPixels = function(points, tolerance = sqrt(.Machine$double.eps)) {
	if (!is(points, "SpatialPoints"))
		stop("points should be of class or extending SpatialPoints")
	is.gridded = gridded(points)
	points = as(points, "SpatialPoints")
	grid = points2grid(points, tolerance)
	if (!is.gridded) {
		points@bbox[,1] = points@bbox[,1] - 0.5 * grid@cellsize
		points@bbox[,2] = points@bbox[,2] + 0.5 * grid@cellsize
	}
	new("SpatialPixels", points, grid = grid, 
		grid.index = getGridIndex(coordinates(points), grid))
}

setMethod("coordinates", "SpatialPixels", function(obj) obj@coords)

SpatialGrid = function(grid, proj4string = CRS(as.character(NA))) {
	pts = boguspoints(grid)
	pts@bbox[,1] = pts@bbox[,1] - 0.5 * grid@cellsize
	pts@bbox[,2] = pts@bbox[,2] + 0.5 * grid@cellsize
	proj4string(pts) = proj4string
	new("SpatialGrid", pts, grid = grid, grid.index = integer(0))
}

setMethod("coordinates", "SpatialGrid", function(obj) coordinates(obj@grid))

getGridTopology = function(obj) {
	if (!is(obj, "SpatialPixels"))
		stop("object is not or does not extend class SpatialPixels")
	obj@grid
}

areaSpatialGrid = function(obj) {
	cellarea = prod(obj@grid@cellsize)
	if (is(obj, "SpatialGrid"))
		return(prod(obj@grid@cells.dim) * cellarea)
	else # take number of cells:
		length(obj@grid.index) * cellarea
}

gridparameters = function(obj) { 
	if (is(obj, "SpatialPixels"))
		obj = obj@grid
	if (is(obj, "GridTopology"))
		return(data.frame(
			cellcentre.offset = obj@cellcentre.offset,
			cellsize = obj@cellsize,
			cells.dim = obj@cells.dim))
	return(numeric(0))
}

boguspoints = function(grid) {
	x = rbind(grid@cellcentre.offset, 
			grid@cellcentre.offset + (grid@cells.dim - 1) * grid@cellsize)
	SpatialPoints(x)
}

getGridIndex = function(cc, grid, all.inside = TRUE) {
	n = ncol(cc)
	idx = rep(1, nrow(cc))
	cumprod = 1
	for (i in 1:n) {
		this.idx = round((cc[,i] - grid@cellcentre.offset[i])/grid@cellsize[i])
		if (i == 2)
			this.idx = grid@cells.dim[2] - (this.idx + 1)
		outside = this.idx >= grid@cells.dim[i] | this.idx < 0
		if (any(outside)) {
			if (all.inside) {
				print(summary(this.idx))
				stop("this.idx out of range")
			} else
				this.idx[outside] = NA
		}
		idx = idx + this.idx * cumprod
		cumprod = cumprod * grid@cells.dim[i]
	}
	outside = idx < 1 | idx > .NumberOfCells(grid)
	if (any(na.omit(outside))) {
		print(summary(idx))
		stop("index outside boundaries")
	}
	as.integer(round(idx))
}

subset.SpatialPixels <- function(x, subset, select, drop = FALSE, ...) {
	xSP <- as(x, "SpatialPoints")
	if (missing(select)) select <- colnames(coordinates(xSP))
	res <- subset(xSP, subset=subset, select=select, drop = drop, ...)
	gridded(res) = TRUE
	res
}

setMethod("[", "SpatialPixels",
	function(x, i, j, ..., drop = TRUE) {
#		if (!missing(drop))
#			stop("don't supply drop: it needs to be FALSE anyway")
		if (!missing(j))
			stop("can only select pixels with a single index")
		if (missing(i))
			return(x)
		if (drop) { # default: adjust bbox and grid
			res = as(x, "SpatialPoints")[i]
			tolerance = list(...)$tolerance
			if (!is.null(tolerance))
				res = SpatialPixels(res, tolerance = tolerance)
			else
				gridded(res) = TRUE
		} else
			res = new("SpatialPixels", bbox = x@bbox, proj4string = x@proj4string,	
				coords = x@coords[i, , drop = FALSE], grid = x@grid, 
				grid.index = x@grid.index[i])
		res
	}
)

setMethod("[", "SpatialGrid",
	function(x, i, j, ..., drop = TRUE) {
		drop <- FALSE
#		if (!missing(drop))
#			stop("don't supply drop: it needs to be FALSE anyway")
		gr = x@grid
		if (missing(i))
			rows = 1:gr@cells.dim[2]
		else
			rows = i
		if (missing(j))
			cols = 1:gr@cells.dim[1]
		else
			cols = j
		idx = 1:prod(gr@cells.dim[1:2])
		m = matrix(idx, gr@cells.dim[2], gr@cells.dim[1], byrow = TRUE)[rows,cols]
		idx = as.vector(m) # t(m)?
		cc = SpatialPixels(SpatialPoints(coordinates(x)[idx,], CRS(proj4string(x))))
		cc = as(cc, "SpatialGrid")
		cc
	}
)

setAs("SpatialPixels", "SpatialGrid", function(from) SpatialGrid(from@grid, from@proj4string))

as.data.frame.SpatialPixels = function(x, row.names, optional)
	as.data.frame(coordinates(x))

as.data.frame.SpatialGrid = as.data.frame.SpatialPixels

setAs("SpatialPixels", "data.frame", function(from) as.data.frame.SpatialPixels(from))
setAs("SpatialGrid", "data.frame", function(from) as.data.frame.SpatialGrid(from))

#setAs("SpatialGrid", "SpatialPixels", function(from)
#	SpatialPixels(SpatialPoints(coordinates(from), from@proj4string))
#)

setMethod("summary", "SpatialPixels", summary.Spatial)
setMethod("summary", "SpatialGrid", summary.Spatial)

print.summary.SpatialPixels = print.summary.Spatial
print.summary.SpatialGrid = print.summary.Spatial

print.SpatialPixels = function(x, ...) {
	cat("Object of class SpatialPixels\n")
	print(summary(x@grid))
	print(as(x, "SpatialPoints"))
	invisible(x)
}
print.SpatialGrid = function(x, ...) {
	cat("Object of class SpatialGrid\n")
	print(summary(x@grid))
	print(as(x, "SpatialPoints"))
	invisible(x)
}
"$<-.SpatialGrid" = function(x,i,value) {
	df = data.frame(value)
	names(df) = as.character(substitute(i))
	SpatialGridDataFrame(x@grid, df) 
}
"$<-.SpatialPixels" = function(x,i,value) { 
	df = data.frame(value)
	names(df) = as.character(substitute(i))
	SpatialPixelsDataFrame(x, df)
}

# make a SpatialPolygons from a SpatialPixels - Kohris Sahlen workshop

as.SpatialPolygons.SpatialPixels <- function(obj, proj4string=CRS(as.character(NA)))
{
	obj_crds <- coordinates(obj)
	IDs <- IDvaluesSpatialPixels(obj)
	nPolygons <- nrow(obj_crds)
	cS <- slot(slot(obj, "grid"), "cellsize")
	cS2 <- cS/2
	cS2x <- cS2[1]
	cS2y <- cS2[2]
	Srl <- vector(mode="list", length=nPolygons)
	for (i in 1:nPolygons) {
		xi <- obj_crds[i,1]
		yi <- obj_crds[i,2]
		x <- c(xi-cS2x, xi-cS2x, xi+cS2x, xi+cS2x, xi-cS2x)
		y <- c(yi-cS2y, yi+cS2y, yi+cS2y, yi-cS2y, yi-cS2y)
		Srl[[i]] <- Polygons(list(Polygon(coords=cbind(x, y)
#, proj4string=proj4string
)), ID=IDs[i])
	}
	res <- as.SpatialPolygons.PolygonsList(Srl, proj4string=proj4string)
	res
}

IDvaluesSpatialPixels <- function(obj) {
	if (!is(obj, "SpatialPixels"))
		stop("function only works for objects of class or extending SpatialPixels")

	cc <- slot(obj, "grid.index")
	res <- as.matrix(sapply(cc, as.integer))
	paste("g", res, sep="")
}
