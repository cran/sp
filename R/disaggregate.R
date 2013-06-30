# copied from raster:
if (!isGeneric("disaggregate")) {
    setGeneric("disaggregate", function(x, ...)
		standardGeneric("disaggregate"))
}

# Robert Hijmans:
explodePolygons <- function(x) {
	npols <- length(x@polygons)
	crs <- x@proj4string
	count <- 0
	p <- NULL
	np <- vector(length=npols)
	for (i in 1:npols) {
		parts <- x@polygons[[i]]@Polygons
		np[i] <- length(parts)
		p <- c(p, sapply(1:np[i], function(x) Polygons(parts[x], count + x)))
		count = count + np[i]
	}
	p <- SpatialPolygons(p)
	proj4string(p) <- crs
	if (.hasSlot(x, 'data')) {
		np <- rep(1:npols, np)
		x <- x@data[np, , drop = FALSE]
		#rownames(x) <- 1:nrow(x)
		rownames(x) <- NULL
		SpatialPolygonsDataFrame(p, x, FALSE)
	} else
		p
}

setMethod("disaggregate", "SpatialPolygons", 
	function(x,...) explodePolygons(x))

setMethod("disaggregate", "SpatialPolygonsDataFrame", 
	function(x,...) explodePolygons(x))

explodeLines <- function(x) {
	nlins <- length(x@lines)
	crs <- x@proj4string
	count <- 0
	p <- NULL
	nl <- vector(length=nlins)
	for (i in 1:nlins) {
		parts <- x@lines[[i]]@Lines
		nl[i] <- length(parts)
		p <- c(p, sapply(1:nl[i], function(x) Lines(parts[x], count + x)))
		count = count + nl[i]
	}
	p <- SpatialLines(p)
	proj4string(p) <- crs
	if (.hasSlot(x, 'data')) {
		nl <- rep(1:nlins, nl)
		x <- x@data[nl, , drop = FALSE]
		rownames(x) <- 1:nrow(x)
		SpatialLinesDataFrame(p, x, FALSE)
	} else
		p
}

setMethod("disaggregate", "SpatialLines", 
	function(x,...) explodeLines(x))

setMethod("disaggregate", "SpatialLinesDataFrame", 
	function(x,...) explodeLines(x))

# Roger, claims Barry wrote it first:
unfoldLines = function(x) {
	crds <- coordinates(x)
	nobjs <- sum(sapply(crds, length))
	out <- vector(mode="list", length=nobjs)
	i <- 1
	for (j in seq(along=crds)) {
  		jcrds <- crds[[j]]
  		for (k in seq(along=jcrds)) {
    		out[[i]] <- Lines(list(Line(jcrds[k])), as.character(i))
    		i <- i + 1
  		}
	}
	SLout <- SpatialLines(out)
	length(SLout) 
}
