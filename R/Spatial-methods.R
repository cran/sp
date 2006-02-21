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
if (!isGeneric("dimensions"))
	setGeneric("dimensions", function(obj)
		standardGeneric("dimensions"))
if (!isGeneric("gridded"))
	setGeneric("gridded", function(obj)
		standardGeneric("gridded"))
if (!isGeneric("overlay"))
	setGeneric("overlay", function(x, y, ...)
		standardGeneric("overlay"))
if (!isGeneric("plot"))
	setGeneric("plot", function(x, y, ...)
		standardGeneric("plot"))
if (!isGeneric("polygons"))
	setGeneric("polygons", function(obj)
		standardGeneric("polygons"))
if (!isGeneric("polygons<-"))
	setGeneric("polygons<-", function(object, value)
		standardGeneric("polygons<-"))
if (!isGeneric("spplot"))
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
if (!isGeneric("spsample"))
	setGeneric("spsample", function(x, n, type, ...)
		standardGeneric("spsample"))
if (!isGeneric("summary"))
	setGeneric("summary", function(object, ...)
		standardGeneric("summary"))
if (as.numeric(version$minor) < 3) {
    if (!isGeneric("transform"))
	setGeneric("transform", function(x, ...)
		standardGeneric("transform"))
} else {
    if (!isGeneric("transform"))
	setGeneric("transform", function(`_data`, ...)
		standardGeneric("transform"))
}
setMethod("bbox", "Spatial", function(obj) obj@bbox)

setMethod("dimensions", "Spatial", function(obj) nrow(bbox(obj)))

setMethod("polygons", "Spatial", function(obj) {
		if (is(obj, "SpatialPolygons"))
			as(obj, "SpatialPolygons")
		else
			stop("polygons method only available for objects of class or deriving from SpatialPolygons")
	}
)

if (as.numeric(version$minor) < 3) {

    transform.Spatial <- function(x, ...) {
	if (require(spproj)) 
		standardGeneric("transform")
	else 
		stop("for using (coordinate) transform on objects deriving from Spatial, library spproj is required")
    }

} else {

    transform.Spatial <- function(`_data`, ...) {
	if (require(spproj)) 
		standardGeneric("transform")
	else 
		stop("for using (coordinate) transform on objects deriving from Spatial, library spproj is required")
    }
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
			|| is(object, "SpatialGridDataFrame") || is(object, "SpatialPolygonsDataFrame"))
        obj[["data"]] = summary(object@data)
    class(obj) = "summary.Spatial"
    obj
}
setMethod("summary", "Spatial", summary.Spatial)

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

# sp.axes = FALSE

#asp <- function(x, ylim) {
#	if (is.na(proj4string(x)) || is.projected(x))
#		return(1.0)
#	else
#		return(1/cos((mean(ylim) * pi)/180))
#}

plot.Spatial <- function(x, xlim=NULL, ylim=NULL, 
		asp = ifelse(is.na(proj4string(x)) || is.projected(x), 1.0, 1/cos((mean(ylim) * pi)/180)), 
		axes = FALSE, bg = par("bg"), ...) {
	bbox <- bbox(x)
	if (is.null(xlim)) xlim <- bbox[1,]
	if (is.null(ylim)) ylim <- bbox[2,]
	frame() # S-Plus compatible version of plot.new()
	if (is.R())
		plot.window(xlim = xlim, ylim = ylim, asp = asp, ...)
	else {
		plot.default(x = bbox[1,], y = bbox[2,], type = "n", 
			xlim = xlim, ylim = ylim, asp = asp, ...)
	}
	pl_reg <- par("usr")
	rect(xleft=pl_reg[1], ybottom=pl_reg[3], xright=pl_reg[2], 
		ytop=pl_reg[4], col=bg, border=FALSE)
	if (axes) { # set up default axes system & box:
		box()
		isp = is.projected(x)
		if (!is.na(isp) && !isp) {
			degAxis(1, ...)
			degAxis(2, ...)
		} else {
			axis(1, ...)
			axis(2, ...)
		}
#		axis(3, labels = FALSE, ...)
#		axis(4, labels = FALSE, ...)
	}
}
setMethod("plot", signature(x = "Spatial", y = "missing"), 
	function(x,y,...) plot.Spatial(x,...))

degAxis = function (side, at, labels, ...) {
		if (missing(at))
        	at = axTicks(side)
        if (missing(labels)) {
			labels = FALSE
        	if (side == 1 || side == 3)
               	labels = parse(text = degreeLabelsEW(at))
        	else if (side == 2 || side == 4)
               	labels = parse(text = degreeLabelsNS(at))
		} 
        axis(side, at = at, labels = labels, ...)
}
