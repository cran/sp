if (!isGeneric("bbox"))
	setGeneric("bbox", function(obj)
		standardGeneric("bbox"))
if (!isGeneric("coordinates"))
	setGeneric("coordinates", function(obj)
		standardGeneric("coordinates"))
if (!isGeneric("coordinates<-"))
	setGeneric("coordinates<-", function(object, value)
		standardGeneric("coordinates<-"))
if (!isGeneric("coordnames"))
	setGeneric("coordnames", function(x)
		standardGeneric("coordnames"))
if (!isGeneric("coordnames<-"))
	setGeneric("coordnames<-", function(x,value)
		standardGeneric("coordnames<-"))
if (!isGeneric("rings"))
	setGeneric("rings", function(obj)
		standardGeneric("rings"))
if (!isGeneric("gridded"))
	setGeneric("gridded", function(obj)
		standardGeneric("gridded"))
if (!isGeneric("dimensions"))
	setGeneric("dimensions", function(obj)
		standardGeneric("dimensions"))
if (!isGeneric("transform"))
	setGeneric("transform", function(x, ...)
		standardGeneric("transform"))
if (!isGeneric("overlay"))
	setGeneric("overlay", function(x, y, ...)
		standardGeneric("overlay"))
if (!isGeneric("spplot"))
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
if (!isGeneric("spsample"))
	setGeneric("spsample", function(x, n, type, ...)
		standardGeneric("spsample"))

setMethod("bbox", "Spatial", function(obj) obj@bbox)

setMethod("dimensions", "Spatial", function(obj) nrow(bbox(obj)))

setMethod("rings", "Spatial", function(obj) {
		if (is(obj, "SpatialRings"))
			as(obj, "SpatialRings")
		else
			stop("rings method only available for objects of class or deriving from SpatialRings")
	}
)

transform.Spatial <- function(x, ...) {
	if (require(spproj)) 
		standardGeneric("transform")
	else 
		stop("for using (coordinate) transform on objects deriving from Spatial, library spproj is required")
}

setMethod("transform", "Spatial", transform.Spatial)

summary.Spatial = function(object, ...) {
    obj = list()
	obj[["class"]] = class(object)
    obj[["bbox"]] = bbox(object)
    obj[["is.projected"]] = is.projected(object)
    obj[["proj4string"]] = object@proj4string@projargs
    if (is(object, "SpatialPoints"))
        obj[["npoints"]] = nrow(object@coords)
	if (is(object, "SpatialGrid"))
		obj[["grid"]] = gridparameters(as(object, "SpatialGrid"))
    if (is(object, "SpatialPointsDataFrame") || is(object, "SpatialLinesDataFrame") 
			|| is(object, "SpatialGridDataFrame") || is(object, "SpatialRingsDataFrame"))
        obj[["data"]] = summary(object@data)
    class(obj) = "summary.Spatial"
    obj
}

print.summary.Spatial = function(x, ...) {
	cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
    cat("Coordinates:\n")
    print(x[["bbox"]])
    cat(paste("Is projected:", x[["is.projected"]], "\n"))
    cat(paste("proj4string : [", x[["proj4string"]], "]\n", sep=""))
    if (!is.null(x$npoints)) {
        cat("Number of points: ")
		cat(x$npoints)
		cat("\n")
	}
    if (!is.null(x$n.polygons)) {
        cat("Number of polygons: ")
		cat(x$n.polygons)
        cat("\n")
    }
	if (!is.null(x$grid)) {
        cat("Grid attributes:\n")
        print(x$grid)
    }
    if (!is.null(x$data)) {
        cat("Data attributes:\n")
        print(x$data)
    }
    invisible(x)
}

plot.Spatial <- function(x, xlim=NULL, ylim=NULL, asp=1, ...) {
	bbox <- x@bbox
	if (is.null(xlim)) xlim <- c(bbox[1,1], bbox[1,2])
	if (is.null(ylim)) ylim <- c(bbox[2,1], bbox[2,2])
	plot.new()
	plot.window(xlim=xlim, ylim=ylim, asp=asp, ...)
}
