Line <- function(coords#, proj4string=CRS(as.character(NA))
) {
#	if (!is.matrix(coords)) coords <- as.matrix(coords)
	coords <- coordinates(coords)
	if (ncol(coords) != 2) stop("coords must be a two-column matrix")
#	if (mode(coords) != "numeric")
#		stop("coordinates should have mode numeric")
#	bbox <- .bboxSlot(coords)
	new("Line", coords = coords
#, bbox = as.matrix(bbox), proj4string = proj4string
	)
}

Lines <- function(slinelist, ID=as.character(NA)) {
	if (is(slinelist, "Line"))
		slinelist = list(slinelist)
	if (any(sapply(slinelist, function(x) !is(x, "Line"))))
		stop("slinelist not a list of Line objects")
#	projargs <- unique(sapply(slinelist, proj4string))
#	if (length(projargs) > 1) 
#		stop("differing projections among Line objects")
#	Sp <- new("Spatial", bbox= .bboxSls(slinelist), proj4string=CRS(projargs))
	new("Lines", Lines = slinelist, ID=ID)
}

SpatialLines <- function(LinesList, proj4string=CRS(as.character(NA))) {
	if (any(sapply(LinesList, function(x) !is(x, "Lines")))) 
		stop("lines not Lines objects")
#	if (length(unique(sapply(LineList, function(x) proj4string(x)))) != 1) 
#		stop("Different projections in list of Line objects")
	Sp <- new("Spatial", 
#bbox = .bboxSls(LineList), 
#		proj4string=CRS(proj4string(LineList[[1]]))
bbox=.bboxCalc(LinesList), proj4string=proj4string)
	res <- new("SpatialLines", Sp, lines=LinesList)
	res
}

LineLength = function(cc) {
	if (is(cc, "Line"))
		cc = coordinates(cc)
	dxy = matrix(apply(cc, 2, diff), ncol = 2)
	sum(sqrt(apply(dxy, 1, function(x) sum(x ** 2))))
}

# NEW
.bboxCalc <- function(lst) {
    rx=range(lapply(lst[[1]]@Lines, function(x) range(x@coords[,1]))[[1]])
    ry=range(lapply(lst[[1]]@Lines, function(x) range(x@coords[,2]))[[1]])
	
	for(i in 1:length(lst))
	{
		x = lst[[i]]
		rxx=range(lapply(x@Lines, function(x) range(x@coords[,1]))[[1]])
		ryy=range(lapply(x@Lines, function(x) range(x@coords[,2]))[[1]])
		rx=range(c(rx,rxx))
		ry=range(c(ry,ryy))
    }
	res=rbind(r1=rx,r2=ry)
    dimnames(res)[[2]] <- c("min", "max")
	res
}

bbox.Lines <- function(obj) {
	rx=range(lapply(obj@Lines, function(x) range(x@coords[,1])))
	ry=range(lapply(obj@Lines, function(x) range(x@coords[,2])))
	res=rbind(r1=rx,r2=ry)
	dimnames(res)[[2]] <- c("min", "max")
	res
}

setMethod("bbox", "Lines", bbox.Lines)

bbox.Line <- function(obj) {
    	rx <- range(obj@coords[,1])
    	ry <- range(obj@coords[,2])
	res=rbind(r1=rx,r2=ry)
   	dimnames(res)[[2]] <- c("min", "max")
	res
}

setMethod("bbox", "Line", bbox.Line)


.bboxSls <- function(lst) {
	x <- sapply(lst, function(x) bbox(x)[1,])
	y <- sapply(lst, function(x) bbox(x)[2,])
	r1 <- range(x)
	r2 <- range(y)
	res <- rbind(r1, r2)
	dimnames(res)[[2]] <- c("min", "max")
	res
}



plotSpatialLines <- function(SL, xlim = NULL, ylim = NULL,
	col = 1, lwd = 1, lty=1, add = FALSE, axes = FALSE, ..., 
	setParUsrBB=FALSE) 
{
#	frame()
#	plot.window(xlim = xlim, ylim = ylim, asp = asp)
	if (! add) 
		plot(as(SL, "Spatial"), xlim = xlim, ylim = ylim,
		    axes = axes, ..., setParUsrBB=setParUsrBB)
	lst <- SL@lines
	for (i in seq(along=lst)) {
		sllst = lst[[i]]@Lines
		for (j in seq(along=sllst)) {
			crds <- coordinates(sllst[[j]])
			if (length(col) != length(lst)) 
				col <- rep(col[1], length(lst))
			if (length(lwd) != length(lst)) 
				lwd <- rep(lwd[1], length(lst))
			if (length(lty) != length(lst)) 
				lty <- rep(lty[1], length(lst))
			lines(crds, col = col[i], lwd = lwd[i], 
				lty = lty[i], ...)
		}
	}
}

setMethod("summary", "SpatialLines", summary.Spatial)

setMethod("plot", signature(x = "SpatialLines", y = "missing"),
	function(x, y, ...) plotSpatialLines(x, ...))

setMethod("coordinates", "Line", function(obj) obj@coords)
setMethod("coordinates", "Lines", function(obj) lapply(obj@Lines, coordinates))
setMethod("coordinates", "SpatialLines", function(obj) lapply(obj@lines, coordinates))

lines.Line = function(x, y = NULL, ...) invisible(lines(coordinates(x), ...))
lines.Lines = function(x, y = NULL, ...) invisible(lapply(x@Lines, 
	function(x, ...) lines(x, ...), ...))
lines.SpatialLines = function(x, y = NULL, ...) invisible(lapply(x@lines, 
	function(x, ...) lines(x, ...), ...))

#"[.SpatialLines" =  function(x, i, j, ..., drop = T) {
setMethod("[", "SpatialLines", function(x, i, j, ..., drop = TRUE) {
	if (!missing(j)) stop("only a single index is allowed for [.SpatialLines")
        if (any(is.na(i))) stop("NAs not permitted in row index")
	SpatialLines(x@lines[i], CRS(proj4string(x)))
})

setMethod("coordnames", signature(x = "SpatialLines"), 
	function(x) coordnames(x@lines[[1]])
)
setMethod("coordnames", signature(x = "Lines"), 
	function(x) coordnames(x@Lines[[1]])
)
setMethod("coordnames", signature(x = "Line"), 
	function(x) dimnames(coordinates(x))[[2]]
)
setReplaceMethod("coordnames", 
	signature(x = "SpatialLines", value = "character"),
	function(x, value) {
		dimnames(x@bbox)[[1]] = value
		for (i in seq(along = x@lines))
			coordnames(x@lines[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames", 
	signature(x = "Lines", value = "character"),
	function(x, value) {
		for (i in seq(along = x@Lines))
			coordnames(x@Lines[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames", 
	signature(x = "Line", value = "character"),
	function(x, value) {
		dimnames(x@coords)[[2]] = value
		x
	}
)

getSpatialLinesMidPoints = function(SL) {
	ret = lapply(SL@lines,
		function(x) sapply(x@Lines, 
			function(X) apply(X@coords, 2, mean)
		)
	)
	ret = t(sapply(ret, function(x) apply(x, 1, mean)))
	SpatialPoints(ret, CRS(proj4string(SL)))
}

LinesLength = function(Ls) sum(sapply(Ls@Lines, function(x) LineLength(x)))

SpatialLinesLengths = function(SL) sapply(SL@lines, LinesLength)
