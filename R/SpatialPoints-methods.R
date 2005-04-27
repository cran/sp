"SpatialPoints" = function(coords, proj4string = CRS(as.character(NA))) {
	coords = coordinates(coords) # checks numeric mode
	if (is.null(dimnames(coords)[[2]]))
		dimnames(coords)[[2]] = paste("coords.x", 1:(dim(coords)[2]), sep = "")
	bbox = t(apply(coords, 2, range))
	dimnames(bbox)[[2]] = c("min", "max")
	new("SpatialPoints", coords = coords, bbox = as.matrix(bbox),
		proj4string = proj4string) # transpose bbox?
}

check.numeric = function(obj) {
	lapply(obj, function(x) { if(!is.numeric(x)) 
		stop("cannot retrieve coordinates from non-numeric elements") })
	obj
}

setMethod("coordinates", "list", function(obj) 
	as.matrix(check.numeric(as.data.frame(obj))))
setMethod("coordinates", "data.frame", function(obj) as.matrix(check.numeric(obj)))
setMethod("coordinates", "matrix", 
	function(obj) {
		if (is.numeric(obj)) 
			obj
		else
			stop("cannot derive coordinates from non-numeric matrix")
	}
)

"print.SpatialPoints" <- function(x, ...)
{
	cat("SpatialPoints:\n")
	print(x@coords)
	cat("Coordinate Reference System (CRS) arguments:", proj4string(x),
		"\n")
}

plot.SpatialPoints = function(x, xlab = "x", ylab = "y", asp = 1, pch = 3, ...) 
{
	cc = coordinates(x)
	nm = dimnames(cc)[[2]]
	if (!is.null(nm)) {
		if (missing(xlab))
			xlab = nm[1]
		if (missing(ylab))
			ylab = nm[2]
	}
	plot(cc[,1], cc[,2], xlab = xlab, ylab = ylab, asp = asp, pch = pch, ...)
}

points.SpatialPoints = function(x, y = NULL, ...) points(coordinates(x), ...)

setMethod("show", "SpatialPoints", function(object) print.SpatialPoints(object))

setMethod("coordinates", "SpatialPoints", function(obj) obj@coords)

as.data.frame.SpatialPoints = function(x, row.names, optional) data.frame(x@coords)

setAs("SpatialPoints", "data.frame", function(from) as.data.frame(from))

subset.SpatialPoints <- function(x, subset, select, drop = FALSE, ...) {
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
	if (!missing(select) && (length(select) < 2)) 
		stop("selecting too few coordinate columns")
	res <- SpatialPoints(subset(coordinates(x), subset, select, 
		drop = drop), proj4string = proj4string(x))
	res
}

#"[.SpatialPoints" =  function(x, i, j, ..., drop = T) {
setMethod("[", "SpatialPoints", function(x, i, j, ..., drop = T) {
	SpatialPoints(coords=x@coords[i, , drop = FALSE], 
	proj4string = CRS(proj4string(x)))
})

summary.SpatialPoints = summary.Spatial

print.summary.SpatialPoints = print.summary.Spatial
