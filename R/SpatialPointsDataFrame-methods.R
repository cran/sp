"SpatialPointsDataFrame" = function(coords, data, coords.nrs = numeric(0), 
		proj4string = CRS(as.character(NA)), match.ID = TRUE) {
	if (!is(coords, "SpatialPoints"))
		coords = coordinates(coords)
	if (match.ID && is.matrix(coords)) {
		cc.ID = dimnames(coords)[[1]]
		if (!is.null(cc.ID) && is(data, "data.frame")) {
			n = nrow(data)
			if (length(unique(cc.ID)) != n)
				stop(
				"nr of unique coords ID's (rownames) not equal to nr of data records")
			data.ID = row.names(data)
			mtch = match(cc.ID, data.ID)
			if (any(is.na(mtch)))
				stop("row.names of data and coords do not match")
			if (length(unique(mtch)) != n)
				stop("row.names of data and dimnames of coords do not match")
			data = data[mtch, ]
		}
	}
	if (!is(coords, "SpatialPoints"))
		coords = SpatialPoints(coords, proj4string = proj4string)
	new("SpatialPointsDataFrame", coords, data = as(data, "AttributeList"),
		coords.nrs = coords.nrs)
}

setMethod("coordinates", "SpatialPointsDataFrame", function(obj) obj@coords)

#setReplaceMethod("coordinates", signature(object = "data.frame", value = "numeric"),
#	coordinates.num)
#coordinates.repl = function(object, value) {

setReplaceMethod("coordinates", signature(object = "data.frame", value = "ANY"),
  function(object, value) {
#"coordinates<-" = function(object, value) {
	coord.numbers = NULL
	#if (!is.list(object))
	#	stop("coordinates can only be set on objects of class data.frame or list")
	if (inherits(value, "formula")) {
		cc = model.frame(value, object) # retrieve coords
		if (dim(cc)[2] == 2) {
			nm = as.character(as.list(value)[[2]])[2:3]
			coord.numbers = match(nm, names(object))
		} else if (dim(cc)[2] == 3) {
			nm = c(as.character(as.list((as.list(value)[[2]])[2])[[1]])[2:3],
				as.character(as.list(value)[[2]])[3])
			coord.numbers = match(nm, names(object))
		} # else: give up.
	} else if (is.character(value)) {
		cc = object[, value] # retrieve coords
		coord.numbers = match(value, names(object))
	} else if (is.null(dim(value)) && length(value) > 1) { # coord.columns?
		if (any(value != as.integer(value) || any(value < 1)))
			stop("coordinate columns should be positive integers")
		cc = object[, value] # retrieve coords
		coord.numbers = value
	} else  # raw coordinates given; try transform them to matrix:
		cc = coordinates(value)
	if (any(is.na(cc)))
		stop("coordinates are not allowed to contain missing values")
	if (!is.null(coord.numbers)) {
		object = object[ , -coord.numbers, drop = FALSE]
		stripped = coord.numbers
		# ... but as.data.frame(x) will merge them back in, so nothing gets lost.
		if (ncol(object) == 0) stop(
			"only coords columns present: use SpatialPoints to create a points object")
	} else
		stripped = numeric(0)
	SpatialPointsDataFrame(data = object, coords = cc, coords.nrs = stripped,
		match.ID = FALSE)
#}
  }
)

print.SpatialPointsDataFrame = function(x, ...) {
	cc = substring(paste(as.data.frame(t(signif(coordinates(x))))),2,999)
	# could be done in S-Plus by unpaste(x, "c")[[2]]
	print(data.frame("coordinates" = cc, x@data), ...)
}

dim.SpatialPointsDataFrame = function(x) dim(x@data)

as.data.frame.SpatialPointsDataFrame = function(x, row.names, optional)  {
#	if (length(x@coords.nrs) > 0) {
#		nc = ncol(x@coords)
#		nd = ncol(x@data)
#		nm = character(nc+nd)
#		ret = list()
#		for (i in 1:nc)
#			ret[[x@coords.nrs[i]]] = x@coords[,i]
#		nm[x@coords.nrs] = dimnames(x@coords)[[2]]
#		idx.new = (1:(nc+nd))[-(x@coords.nrs)]
#		for (i in 1:nd)
#			ret[[idx.new[i]]] = x@data[,i]
#		nm[idx.new] = names(x@data)
#		names(ret) = nm
#		data.frame(ret)
#	} else
	if (length(x@coords.nrs) > 0) {
		maxi = max(x@coords.nrs, (ncol(x@data) + ncol(x@coords)))
		ret = list()
		for (i in 1:ncol(x@coords))
			ret[[x@coords.nrs[i]]] = x@coords[,i]
		names(ret)[x@coords.nrs] = dimnames(x@coords)[[2]]
		idx.new = (1:maxi)[-(x@coords.nrs)]
		for (i in 1:ncol(x@data))
			ret[[idx.new[i]]] = x@data[,i]
		names(ret)[idx.new] = names(x@data)
		ret = ret[unlist(lapply(ret, function(x) !is.null(x)))]
		data.frame(ret)
	} else
		data.frame(x@data, x@coords)
}

setAs("SpatialPointsDataFrame", "data.frame", function(from)
	as.data.frame.SpatialPointsDataFrame(from))

names.SpatialPointsDataFrame <- function(x) names(x@data)
"names<-.SpatialPointsDataFrame" <- function(x, value) { names(x@data) = value; x }

#"coordnames<-.SpatialPointsDataFrame" <- function(x, value)

ShowSpatialPointsDataFrame = function(object) print.SpatialPointsDataFrame(object)

setMethod("show", "SpatialPointsDataFrame", ShowSpatialPointsDataFrame)

plot.SpatialPointsDataFrame = function(x, ...) {
	plot(as(x, "SpatialPoints"), ...)
}

points.SpatialPointsDataFrame = function(x, y = NULL, ...) 
	points(as(x, "SpatialPoints"), ...)

summary.SpatialPointsDataFrame = function(object, ...) {
    obj = list()
	obj[["data"]] = summary(object@data)
	obj[["coords"]] = summary(object@coords)
	obj[["proj"]] = proj4string(object)
    class(obj) = "summary.SpatialPointsDataFrame"
    obj
}

print.summary.SpatialPointsDataFrame = function(x, ...) {
	cat("attribute table data:\n")
	print(x$data)
	cat("Coordinates:\n")
	print(x$coords)
	cat("Coordinate Reference System (CRS) arguments:", x$proj, "\n")
	cat("\n")
}

subset.SpatialPointsDataFrame <- function(x, subset, select, 
		drop = FALSE, ...) {
    if (version$major == 2 & version$minor < 1 ) {
	subset.matrix <- function (x, subset, select, drop = FALSE, ...) {
    		if (missing(select)) 
        		vars <- TRUE
    		else {
        		nl <- as.list(1:ncol(x))
        		names(nl) <- colnames(x)
        		vars <- eval(substitute(select), nl, parent.frame())
    		}
    		if (!is.logical(subset)) 
        		stop("'subset' must be logical")
    		x[subset & !is.na(subset), vars, drop = drop]
		}
    }
	xSP <- coordinates(x)
	dfSP <- as.data.frame(x)
	cselect <- colnames(xSP)
	points <- subset(xSP, subset=subset, select=cselect, drop = drop, ...)
	if (missing(select)) select <- names(dfSP)
	data <- subset(dfSP, subset=subset, select=select, drop = drop, ...)
	SPDF <- SpatialPointsDataFrame(points, data, 
		proj4string = CRS(proj4string(x)))
	SPDF
}

#"[.SpatialPointsDataFrame" <- function(x, i, j, ... , drop = FALSE) {
setMethod("[", "SpatialPointsDataFrame", function(x, i, j, ... , drop = FALSE) {
	missing.i = missing(i)
	missing.j = missing(j)
	if (drop)
		stop("coerce to data.frame first for drop = TRUE")
	nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
	if (missing.i && missing.j) {
		i = TRUE
		j = TRUE
	} else if (missing.j && !missing.i) { 
		if (nargs == 2) {
			j = i
			i = TRUE
		} else {
			j = TRUE
		}
	} else if (missing.i && !missing.j)
		i = TRUE
	if (is.matrix(i))
		stop("matrix argument not supported in SpatialPointsDataFrame selection")
	SpatialPointsDataFrame(coords = x@coords[i, , drop = FALSE],
		data = x@data[i, j, drop = FALSE], 
		coords.nrs = x@coords.nrs, 
		proj4string = CRS(proj4string(x)))
})

"[[.SpatialPointsDataFrame" =  function(x, ...)
#setMethod("[[", "SpatialPointsDataFrame", function(x, ...)
	x@data[[...]]
#)

"[[<-.SpatialPointsDataFrame" =  function(x, i, j, value) {
	if (!missing(j))
		stop("only valid calls are x[[i]] <- value")
	if (is.character(i) && any(!is.na(match(i, dimnames(coordinates(x))[[2]]))))
		stop(paste(i, "is already present as a coordinate name!"))
	x@data[[i]] <- value
	x
}

"$.SpatialPointsDataFrame" = function(x, name) x@data[[name]]

"$<-.SpatialPointsDataFrame" = function(x, i, value) { x@data[[i]] = value; x }

summary.SpatialPointsDataFrame = summary.Spatial
