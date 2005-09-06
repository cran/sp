SpatialPixels = function(points, tolerance = 10 * .Machine$double.eps) {
	if (!is(points, "SpatialPoints"))
		stop("points should be of class or extending SpatialPoints")
	points = as(points, "SpatialPoints")
	grid = points2grid(points, tolerance)
	points@bbox[1,1] = points@bbox[1,1] - 0.5 * grid@cellsize[1]
	points@bbox[1,2] = points@bbox[1,2] + 0.5 * grid@cellsize[1]
	points@bbox[2,1] = points@bbox[2,1] - 0.5 * grid@cellsize[2]
	points@bbox[2,2] = points@bbox[2,2] + 0.5 * grid@cellsize[2]
	new("SpatialPixels", points, grid = grid, 
		grid.index = getGridIndex(coordinates(points), grid))
}

setMethod("coordinates", "SpatialPixels", function(obj) obj@coords)

SpatialGrid = function(grid, proj4string = CRS(as.character(NA))) {
	pts = boguspoints(grid)
	pts@bbox[1,1] = pts@bbox[1,1] - 0.5 * grid@cellsize[1]
	pts@bbox[1,2] = pts@bbox[1,2] + 0.5 * grid@cellsize[1]
	pts@bbox[2,1] = pts@bbox[2,1] - 0.5 * grid@cellsize[2]
	pts@bbox[2,2] = pts@bbox[2,2] + 0.5 * grid@cellsize[2]
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
	cellarea = prod(obj@grid@cellsize[1:2])
	if (is(obj, "SpatialGrid"))
		return(prod(obj@grid@cells.dim) * cellarea)
	else
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
		drop <- FALSE
#		if (!missing(drop))
#			stop("don't supply drop: it needs to be FALSE anyway")
		if (!missing(j))
			stop("can only select pixels with a single index")
		if (missing(i))
			return(x)
		res = as(x, "SpatialPoints")[i]
		gridded(res) = TRUE
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
