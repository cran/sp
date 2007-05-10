SpatialPolygons <- function(Srl, pO, proj4string=CRS(as.character(NA))) {
	bb <- .bboxCalcR(Srl)
	if (missing(pO)) {
		area <- sapply(Srl, function(x) x@area)
		pO <- as.integer(order(area, decreasing=TRUE))
	}
	Sp <- new("Spatial", bbox=bb, proj4string=proj4string)
	res <- new("SpatialPolygons", Sp, polygons=Srl, plotOrder=as.integer(pO))
	res
}

Polygon <- function(coords, hole=as.logical(NA)) {
	
	coords <- coordinates(coords)
#	if (!is.matrix(coords)) stop("coords must be a two-column matrix")
	if (ncol(coords) != 2) stop("coords must be a two-column matrix")
	cG <- .spFindCG(coords)
	rD <- cG$rD
	if (is.na(hole)) {
		hole <- FALSE
		if (rD < 0) hole <- TRUE
	} else {
		if (hole && rD > 0) coords <- coords[nrow(coords):1,]
		if (!hole && rD < 0) coords <- coords[nrow(coords):1,]
	}
	rD <- .spFindCG(coords)$rD
	sl <- Line(coords)
#	rD <- .ringDirxy(coordinates(sl))
#	cents <- .RingCentrd_2d(coordinates(sl))
#	.saneRD(rD)
	res <- new("Polygon", sl, labpt=cG$cents, 
		area=cG$area, hole=as.logical(hole), 
		ringDir=as.integer(rD))
	res
}

Polygons <- function(srl, ID) {
	if (any(sapply(srl, function(x) !is(x, "Polygon"))))
		stop("srl not a list of Polygon objects")
#	projargs <- unique(sapply(srl, proj4string))
#	if (length(projargs) > 1) 
#		stop("differing projections among Polygon objects")
	if (missing(ID)) stop("Single ID required")
	if (length(ID) != 1) stop("Single ID required")

#	nParts <- length(srl)
# check their plot order
	areas <- sapply(srl, getPolygonAreaSlot)
	pO <- order(areas, decreasing=TRUE)
	holes <- sapply(srl, function(x) x@hole)
	marea <- which.max(areas)
	which_list <- ifelse(length(srl) == 1, 1, marea)
	if (holes[which_list]) {
		crds <- srl[[which_list]]@coords
		srl[[which_list]] <- Polygon(coords=crds[nrow(crds):1,])
	}
	Sarea <- sum(abs(areas))
# assign label point to the largest member ring
	lpt <- t(sapply(srl, getPolygonLabptSlot))
	labpt <- lpt[which_list,]
		
#	Sp <- new("Spatial", bbox=.bboxSrs(srl), proj4string=CRS(projargs))
	res <- new("Polygons",
		Polygons=srl, plotOrder=as.integer(pO),
		labpt=as.numeric(labpt), ID=as.character(ID), area=Sarea)
	res

}

bbox.Polygons <- function(obj) {
	rx=range(c(sapply(obj@Polygons, function(x) range(x@coords[,1]))))
	ry=range(c(sapply(obj@Polygons, function(x) range(x@coords[,2]))))
	res=rbind(r1=rx,r2=ry)
   	colnames(res) <- c("min", "max")
	res
}

setMethod("bbox", "Polygons", bbox.Polygons)

bbox.Polygon <- function(obj) {
	rx <- range(obj@coords[,1])
   	ry <- range(obj@coords[,2])
	res=rbind(r1=rx,r2=ry)
    colnames(res) <- c("min", "max")
	res
}

setMethod("bbox", "Polygon", bbox.Polygon)

as.SpatialPolygons.PolygonsList <- function(Srl, proj4string=CRS(as.character(NA))) {
	if (any(sapply(Srl, function(x) !is(x, "Polygons"))))
		stop("srl not a list of Polygons objects")
#	projargs <- unique(sapply(Srl, proj4string))
#	if (length(projargs) > 1) 
#		stop("differing projections among Polygons objects")

#	n <- length(Srl)

	res <- SpatialPolygons(Srl, proj4string=proj4string)
	res
}

setMethod("[", "SpatialPolygons", function(x, i, j, ..., drop = TRUE) {
	if (is.logical(i)) {
		if (length(i) == 1 && i)
			i = 1:length(x@polygons)
		else
			i <- which(i)
	}
	if (any(is.na(i)))
		stop("NAs not permitted in row index")
	if (length(unique(i)) != length(i))
		stop("SpatialPolygons selection: can't find plot order if polygons are replicated")
	xx <- SpatialPolygons(x@polygons[i], proj4string=CRS(proj4string(x)))
	xx
#	x@polygons = x@polygons[i]
#	x@bbox <- .bboxCalcR(x@polygons)
#	area <- sapply(slot(x, "polygons"), function(i) slot(i, "area"))
#	x@plotOrder <- as.integer(order(area, decreasing=TRUE))
#	x
})

setMethod("coordnames", signature(x = "SpatialPolygons"), 
	function(x) coordnames(x@polygons[[1]])
)
setMethod("coordnames", signature(x = "Polygons"), 
	function(x) coordnames(x@Polygons[[1]])
)
setMethod("coordnames", signature(x = "Polygon"), 
	function(x) dimnames(x@coords)[[2]]
)
setReplaceMethod("coordnames", 
	signature(x = "SpatialPolygons", value = "character"),
	function(x, value) {
		dimnames(x@bbox)[[1]] = value
		for (i in seq(along = x@polygons))
			coordnames(x@polygons[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames", 
	signature(x = "Polygons", value = "character"),
	function(x, value) {
		for (i in seq(along = x@Polygons))
			coordnames(x@Polygons[[i]]) = value
		x
	}
)
setReplaceMethod("coordnames", 
	signature(x = "Polygon", value = "character"),
	function(x, value) {
		dimnames(x@coords)[[2]] = value
		x
	}
)

setMethod("coordinates", "SpatialPolygons", 
	function(obj) getSpPPolygonsLabptSlots(obj))

getSpatialPolygonsLabelPoints = function(SP) {
	ret = t(sapply(slot(SP, "polygons"), function(x) slot(x, "labpt")))
	SpatialPoints(ret, CRS(proj4string(SP)))
}

as.Lines.Polygons = function(from) {
	lst = lapply(from@Polygons, function(x) as(x, "Line"))
	Lines(lst, from@ID)
}
setAs("Polygons", "Lines", as.Lines.Polygons)

as.SpatialLines.SpatialPolygons = function(from)
	SpatialLines(lapply(from@polygons, function(x) as(x, "Lines")),
		CRS(proj4string(from)))

setAs("SpatialPolygons", "SpatialLines", as.SpatialLines.SpatialPolygons)
