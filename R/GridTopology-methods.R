GridTopology = function(cellcentre.offset, cellsize, cells.dim) {
	new("GridTopology",
		cellcentre.offset = cellcentre.offset,
		cellsize = cellsize,
		cells.dim = as.integer(cells.dim))
}

# setMethod("show", "GridTopology", function(object) summary(object))

as.data.frame.GridTopology = function(x, row.names, optional, ...) data.frame(
		cellcentre.offset = x@cellcentre.offset,
		cellsize = x@cellsize,
		cells.dim = x@cells.dim
	)

setAs("GridTopology", "data.frame", function(from) as.data.frame.GridTopology(from))

setMethod("coordinates", "GridTopology", function(obj) {
	cc = do.call("expand.grid", coordinatevalues(obj))
#	as.matrix(sapply(cc, as.numeric))
# dropping dimension for single cell grid
	do.call("cbind", lapply(cc, as.numeric))
})

coordnamesGT = function(x, value) {
	names(x@cellcentre.offset) = value
	names(x@cellsize) = value
	names(x@cells.dim) = value
	x
}

setReplaceMethod("coordnames", 
	signature(x = "GridTopology", value = "character"), coordnamesGT)

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

points2grid = function(points, tolerance=sqrt(.Machine$double.eps), round=NULL, fuzz.tol=3) {
	# work out grid topology from points
	n = dimensions(points)
	ret = new("GridTopology", 
		cellcentre.offset = numeric(n),
		cellsize = numeric(n),
		cells.dim = as.integer(rep(1,n)))
	cc = coordinates(points)
	nr <- nrow(cc)
	for (i in 1:n) { # loop over x, y, and possibly z
		x = cc[, i]
    		sux = sort(unique(x))
		fuzz <- nr/length(sux) < fuzz.tol
    		difx = diff(sux)
		if (length(difx) == 0)
			stop(paste("cannot determine cell size from constant coordinate", i))
		#ru.difx = range(unique(difx))
		ru.difx = range(unique(difx)) # min to max x coord leaps
    		err1 = diff(ru.difx) #?? /max(range(abs(sux))) # (max-min)/max(abs(x))
    		if (err1 > tolerance) { 
			xx = ru.difx / min(ru.difx)
			err2 = max(abs(floor(xx) - xx)) # is it an integer multiple?
			if (err2 > tolerance) {
			  cat(paste("suggested tolerance minimum:", err2, "\n"))
       			stop(paste("dimension", i,": coordinate intervals are not constant"))
			} else if (fuzz) {
				o <- kmeans(difx, 2)
				mdx <- which.max(o$centers)
				difx_in <- difx[o$cluster == mdx]
				dxsd <- sd(difx_in)
				if (dxsd > tolerance) {
				    warning(paste("grid has empty column/rows in dimension", i))
				    hh <- hist(difx_in, plot=FALSE)
				    hh1 <- which.max(hh$counts)
				    if (hh$counts[hh1] < sum(hh$counts)/2)
				     stop(paste("dimension", i,
				     ": coordinate intervals are not constant",
				     "after allowing for numeric fuzz"))
				    hh1a <- difx_in >= hh$breaks[hh1]
				    hh1b <- difx_in <= hh$breaks[(hh1+1)]
				    difx <- mean(difx_in[hh1a & hh1b])
				} else difx <- max(o$centers)
				if (!is.null(round))
				    difx <- round(difx, digits=round)
			} else {
			    difx = difx[difx < ru.difx[1] + tolerance]
			    warning(paste("grid has empty column/rows in dimension", i))
			}
		}
		ret@cellsize[i] = mean(difx)
		ret@cellcentre.offset[i] = min(sux)
    		ret@cells.dim[i] = as.integer(round(diff(range(sux))/ret@cellsize[i]) + 1) 
			#was: length(sux), but this will not cope with empty rows.
		if (ret@cells.dim[i] > nr/2) 
		warning(paste("grid topology may be corrupt in dimwnsion", i))
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
	if (!is.null(names(x@cellcentre.offset)))
		names(res) = names(x@cellcentre.offset)
	print(res)
	invisible(res)
}
setMethod("show", "GridTopology", function(object) print.GridTopology(object))

summ.GridTopology = function(object, ...) {
	ret = list()
	ret[["values"]] = gridparameters(object)
	class(ret) = "summary.GridTopology"
	ret
}
setMethod("summary", "GridTopology", summ.GridTopology)

print.summary.GridTopology = function(x, ...) {
	cat("Grid topology:\n")
	print(x$values)
	invisible(x)
}

# make a SpatialPolygons from a GridTopology - NERSC query

as.SpatialPolygons.GridTopology <- function(grd, proj4string=CRS(as.character(NA)))
{
	grd_crds <- coordinates(grd)
	IDs <- IDvaluesGridTopology(grd)
	nPolygons <- nrow(grd_crds)
	cS <- grd@cellsize
	cS2 <- cS/2
	cS2x <- cS2[1]
	cS2y <- cS2[2]
	Srl <- vector(mode="list", length=nPolygons)
	for (i in 1:nPolygons) {
		xi <- grd_crds[i,1]
		yi <- grd_crds[i,2]
		x <- c(xi-cS2x, xi-cS2x, xi+cS2x, xi+cS2x, xi-cS2x)
		y <- c(yi-cS2y, yi+cS2y, yi+cS2y, yi-cS2y, yi-cS2y)
		Srl[[i]] <- Polygons(list(Polygon(coords=cbind(x, y)
#, proj4string=proj4string
)), ID=IDs[i])
	}
	res <- SpatialPolygons(Srl, proj4string=proj4string)
	res
}
setAs("GridTopology", "SpatialPolygons", function(from)
	as.SpatialPolygons.GridTopology(from))

# mostly copied from coordinates() for GridTopology, 
# makes IDs "c(i)r(j)" matching the coordinates
# used with SpatialRing-ed grids for the data frame rowname()

IDvaluesGridTopology <- function(obj) {
	if (!is(obj, "GridTopology"))
		stop("function only works for objects of class or extending GridTopology")
	cc <- getGridIndex(coordinates(obj), obj)
	res <- as.matrix(sapply(cc, as.integer))
	paste("g", res, sep="")
}

