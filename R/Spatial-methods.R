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

bbox.default <- function(obj) {
	is_points <- function(obj) {
	    is <- FALSE
	    if(is.array(obj))
		if(length(dim(obj))==2)
			if(dim(obj)[2]>=2) is <- TRUE
	    is
	}
	if(!is_points(obj))stop('object not a >= 2-column array')
	xr <- range(obj[,1],na.rm=TRUE)
	yr <- range(obj[,2],na.rm=TRUE)
	res <- rbind(x=xr, y=yr)
	colnames(res) <- c("min","max")
	res
}


setMethod("bbox", "ANY", bbox.default)

setMethod("bbox", "Spatial", function(obj) obj@bbox)

setMethod("dimensions", "Spatial", function(obj) nrow(bbox(obj)))

setMethod("polygons", "Spatial", function(obj) {
		if (is(obj, "SpatialPolygons"))
			as(obj, "SpatialPolygons")
		else
			stop("polygons method only available for objects of class or deriving from SpatialPolygons")
	}
)

summary.Spatial = function(object, ...) {
    obj = list()
	obj[["class"]] = class(object)
    obj[["bbox"]] = bbox(object)
    obj[["is.projected"]] = is.projected(object)
    obj[["proj4string"]] = object@proj4string@projargs
    if (is(object, "SpatialPoints"))
        obj[["npoints"]] = nrow(object@coords)
	if (is(object, "SpatialGrid"))
		obj[["grid"]] = gridparameters(object)
	if ("data" %in% slotNames(object))
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
#    cat(paste("proj4string : [", x[["proj4string"]], "]\n", sep=""))
    pst <- paste(strwrap(x[["proj4string"]]), collapse="\n")
    if (nchar(pst) < 40) cat(paste("proj4string : [", pst, "]\n", sep=""))
    else cat(paste("proj4string :\n[", pst, "]\n", sep=""))
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
	asp = NA, axes = FALSE, bg = par("bg"), ..., setParUsrBB=FALSE) {
	bbox <- bbox(x)
	if (is.null(xlim)) xlim <- bbox[1,]
	if (is.null(ylim)) ylim <- bbox[2,]
	if (is.na(asp)) asp <- ifelse(is.na(proj4string(x)) || is.projected(x),
		1.0, 1/cos((mean(ylim) * pi)/180))
	frame() # S-Plus compatible version of plot.new()
	if (is.R()) {
		plot.window(xlim = xlim, ylim = ylim, asp = asp, ...)
		if (setParUsrBB) par(usr=c(xlim, ylim))
	} else {
		plot.default(x = bbox[1,], y = bbox[2,], type = "n", 
			xlim = xlim, ylim = ylim, asp = asp, 
			ann=FALSE, axes=FALSE, ...)
		if (setParUsrBB) par(usr=c(xlim, ylim))
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

setReplaceMethod("coordinates", signature(object = "Spatial", value = "ANY"),
	function(object, value) 
		stop("setting coordinates cannot be done on Spatial objects, where they have already been set")
)

setMethod("[[", c("Spatial", "ANY", "missing"), 
	function(x, i, j, ...) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		x@data[[i]]
	}
)

setReplaceMethod("[[", c("Spatial", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		if (is.character(i) && any(!is.na(match(i, dimnames(coordinates(x))[[2]]))))
			stop(paste(i, "is already present as a coordinate name!"))
		x@data[[i]] <- value
		x
	}
)

setMethod("$", c("Spatial", "character"), 
	function(x, name) {
		if (!("data" %in% slotNames(x)))
			stop("no $ method for object without attributes")
		x@data[[name]]
	}
)

setReplaceMethod("$", c("Spatial", "character", "ANY"), 
	function(x, name, value) { 
		if (!("data" %in% slotNames(x)))
			stop("no $<- method for object without attributes")
		x@data[[name]] = value 
		x 
	}
)
