GridTopology = function(cellcentre.offset, cellsize, cells.dim) {
	new("GridTopology",
		cellcentre.offset = cellcentre.offset,
		cellsize = cellsize,
		cells.dim = as.integer(cells.dim))
}

setMethod("show", "GridTopology", function(object) summary(obj))

setMethod("coordinates", "GridTopology", function(obj) {
	cc = do.call("expand.grid", coordinatevalues(obj))
	as.matrix(sapply(cc, as.numeric))
})

coordinatevalues = function(obj) {
	if (!is(obj, "GridTopology"))
		stop("function only works for objects of class or extending GridTopology")
	ret = list()
	for (i in seq(along=obj@cells.dim)) {
		if (i == 2) # y-axis is the exception--starting at top of map, and decreasing:
			ret[[i]] = obj@cellcentre.offset[i] + 
				obj@cellsize[i] * ((obj@cells.dim[i] - 1):0)
		else
			ret[[i]] = obj@cellcentre.offset[i] + 
				obj@cellsize[i] * (0:(obj@cells.dim[i] - 1))
	}
	ns = names(obj@cellcentre.offset)
	if (is.null(ns))
		ns = paste("s", 1:length(ret), sep = "") #dimnames(obj@bbox)[[1]]
	names(ret) = ns
	ret
}

points2grid = function(points, tolerance) {
	# work out grid topology from points
	n = dimensions(points)
	ret = new("GridTopology", 
		cellcentre.offset = numeric(n),
		cellsize = numeric(n),
		cells.dim = as.integer(rep(1,n)))
	cc = coordinates(points)
	for (i in 1:n) { # loop over x, y, and possibly z
		Warn = TRUE
		x = cc[, i]
    	sux = sort(unique(x))
    	difx = diff(sux)
		if (length(difx) == 0)
			stop(paste("cannot determine cell size from constant coordinate", i))
		ru.difx = range(unique(difx))
    	err1 = diff(ru.difx)/max(range(abs(sux)))
    	if (err1 > tolerance) { 
			xx = ru.difx / min(ru.difx)
			err2 = max(abs(floor(xx) - xx))
			if (err2 > tolerance) {
				cat(paste("suggested tolerance minimum:", err2))
       			stop(paste("dimension", i,": coordinate intervals are not constant"))
			} else if (Warn) {
				warning(paste("grid has empty column/rows in dimension", i))
				Warn = FALSE # warn once per dimension
			}
		}
		ret@cellsize[i] = mean(difx)
		ret@cellcentre.offset[i] = min(sux)
    	ret@cells.dim[i] = length(sux)
	}
	nm = dimnames(cc)[[2]]
	names(ret@cellsize) = nm
	names(ret@cellcentre.offset) = nm
	names(ret@cells.dim) = nm
	ret
}

.NumberOfCells = function(x) {
	if (!is(x, "GridTopology"))
		stop(".NumberOfCells only works on objects of class GridTopology")
	prod(x@cells.dim)
}

print.GridTopology = function(x, ...) {
	res = data.frame(rbind(x@cellcentre.offset, x@cellsize, as.numeric(x@cells.dim)))
	rownames(res) = c("cellcentre.offset", "cellsize", "cells.dim")
	print(res)
	invisible(res)
}

summary.GridTopology = function(object, ...) {
	ret = list()
	ret[["values"]] = gridparameters(object)
	class(ret) = "summary.GridTopology"
	ret
}

print.summary.GridTopology = function(x, ...) {
	cat("Grid topology:\n")
	print(x$values)
	invisible(x)
}
