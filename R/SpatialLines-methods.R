Sline <- function(coords, proj4string=CRS(as.character(NA))) {
	if (!is.matrix(coords)) coords <- as.matrix(coords)
	if (mode(coords) != "numeric")
		stop("coordinates should have mode numeric")
	bbox <- .bboxSlot(coords)
	new("Sline", coords = coords, bbox = as.matrix(bbox),
		proj4string = proj4string)
}

Slines <- function(slinelist) {
	if (is(slinelist, "Sline"))
		slinelist = list(slinelist)
	if (any(sapply(slinelist, function(x) !is(x, "Sline"))))
		stop("slinelist not a list of Sline objects")
	projargs <- unique(sapply(slinelist, proj4string))
	if (length(projargs) > 1) 
		stop("differing projections among Sline objects")
	Sp <- new("Spatial", bbox= .bboxSls(slinelist), proj4string=CRS(projargs))
	new("Slines", Sp, Slines = slinelist)
}

SpatialLines <- function(SlineList) {
	if (any(sapply(SlineList, function(x) !is(x, "Slines")))) 
		stop("lines not Slines objects")
	if (length(unique(sapply(SlineList, function(x) proj4string(x)))) != 1) 
		stop("Different projections in list of Sline objects")
	Sp <- new("Spatial", bbox = .bboxSls(SlineList), 
		proj4string=CRS(proj4string(SlineList[[1]])))
	res <- new("SpatialLines", Sp, lines=SlineList)
	res
}

SlineLength = function(cc) {
	dxy = apply(cc, 2, diff)
	sqrt(sum(apply(dxy, 1, function(x) sum(x ** 2))))
}

.bboxSls <- function(lst) {
	x <- sapply(lst, function(x) bbox(x)[1,])
	y <- sapply(lst, function(x) bbox(x)[2,])
	r1 <- range(x)
	r2 <- range(y)
	res <- rbind(r1, r2)
	colnames(res) <- c("min", "max")
	res
}

.contourLines2SlineList <- function(cL, proj4string=CRS(as.character(NA))) {
	n <- length(cL)
	res <- vector(mode="list", length=n)
	for (i in 1:n) {
		crds <- cbind(cL[[i]][[2]], cL[[i]][[3]])
		res[[i]] <- Sline(coords=crds, proj4string=proj4string)
	}
	res
}


contourLines2SLDF <- function(cL, proj4string=CRS(as.character(NA))) {
	if (length(cL) < 1) stop("cL too short")
	cLstack <- tapply(1:length(cL), sapply(cL, function(x) x[[1]]), 
		function(x) x, simplify=FALSE)
	df <- data.frame(level=names(cLstack))
	m <- length(cLstack)
	res <- vector(mode="list", length=m)
	for (i in 1:m) {
		res[[i]] <- Slines(.contourLines2SlineList(cL[cLstack[[i]]], 
			proj4string=proj4string))
	}
	SL <- SpatialLines(res)
	res <- SpatialLinesDataFrame(SL, data=df)
	res
}

arcobj2SLDF <- function(arc, proj4string=CRS(as.character(NA))) {
	df <- data.frame(arc[[1]])
	n <- length(arc[[2]])
	SlinesList <- vector(mode="list", length=n)
	for (i in 1:n) {
		crds <- cbind(arc[[2]][[i]][[1]], arc[[2]][[i]][[2]])
		SlinesList[[i]] <- Slines(list(Sline(coords=crds, 
			proj4string=proj4string)))
	}
	SL <- SpatialLines(SlinesList)
	res <- SpatialLinesDataFrame(SL, data=df)
	res
}

shp2SLDF <- function(shp, proj4string=CRS(as.character(NA))) {
	if (class(shp) != "Map") stop("shp not a Map object")
	if (attr(shp$Shapes, "shp.type") != "arc") stop("not an arc Map object")
	df <- shp$att.data
	shapes <- shp$Shapes
	n <- length(shapes)
	SlinesList <- vector(mode="list", length=n)
	for (i in 1:n) {
		SlinesList[[i]] <- .shapes2SlinesList(shapes[[i]], 
			proj4string=proj4string)
	}
	SL <- SpatialLines(SlinesList)
	res <- SpatialLinesDataFrame(SL, data=df)
	res
}

.shapes2SlinesList <- function(shape, proj4string=CRS(as.character(NA))) {
	nParts <- attr(shape, "nParts")
	Pstart <- shape$Pstart
	nVerts <- nrow(shape$verts)
	from <- integer(nParts)
	to <- integer(nParts)
	from[1] <- 1
	for (j in 1:nParts) {
		if (j == nParts) to[j] <- nVerts
		else {
			to[j] <- Pstart[j+1]
			from[j+1] <- to[j]+1
		}
	}
	res <- vector(mode="list", length=nParts)
	for (i in 1:nParts) {
		res[[i]] <- Sline(coords=shape$verts[from[j]:to[j],], 
			proj4string=proj4string)
	}
	Slines <- Slines(res)
	Slines
}

plotSpatialLines <- function(SL, xlim = NULL, ylim = NULL, asp = 1, 
	col = 1, add=FALSE, ...) 
{
#	frame()
#	plot.window(xlim = xlim, ylim = ylim, asp = asp)
	if (!add) plot.Spatial(SL, xlim=xlim, ylim=ylim, asp=asp, ...)
	lst <- SL@lines
	for (i in seq(along=lst)) {
		sllst = lst[[i]]@Slines
		for (j in seq(along=sllst)) {
			crds <- coordinates(sllst[[j]])
			if (length(col) == length(lst))
				lines(crds, col = col[i], ...)
			else
				lines(crds, col = col[1], ...)
		}
	}
}

summary.SpatialLines = summary.Spatial

plot.SpatialLines = function(x, ...) plotSpatialLines(x, ...)

setMethod("coordinates", "Sline", function(obj) obj@coords)
setMethod("coordinates", "Slines", function(obj) lapply(obj@Slines, coordinates))
setMethod("coordinates", "SpatialLines", function(obj) lapply(obj@lines, coordinates))

lines.Sline = function(x, y = NULL, ...) invisible(lines(coordinates(x), ...))
lines.Slines = function(x, y = NULL, ...) invisible(lapply(x@Slines, 
	function(x, ...) lines(x, ...), ...))
lines.SpatialLines = function(x, y = NULL, ...) invisible(lapply(x@lines, 
	function(x, ...) lines(x, ...), ...))

#"[.SpatialLines" =  function(x, i, j, ..., drop = T) {
setMethod("[", "SpatialLines", function(x, i, j, ..., drop = T) {
	if (!missing(j)) stop("only a single index is allowed for [.SpatialLines")
	SpatialLines(x@lines[i])
})

setMethod("coordnames", signature(x = "SpatialLines"), 
	function(x) coordnames(x@lines[[1]])
)
setMethod("coordnames", signature(x = "Slines"), 
	function(x) coordnames(x@Slines[[1]])
)
setMethod("coordnames", signature(x = "Sline"), 
	function(x) dimnames(coordinates(x))[[2]]
)
setReplaceMethod("coordnames", 
	signature(x = "SpatialLines", value = "character"),
	function(x, value) {
		for (i in seq(along = x@lines))
			coordnames(x@lines[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames", 
	signature(x = "Slines", value = "character"),
	function(x, value) {
		for (i in seq(along = x@Slines))
			coordnames(x@Slines[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames", 
	signature(x = "Sline", value = "character"),
	function(x, value) {
		dimnames(x@coords)[[2]] = value
		x
	}
)
