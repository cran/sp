# to be moved to glue with maptools:

as.SpatialPolygons.Shapes <- function(shapes, IDs, 
	proj4string=CRS(as.character(NA))) {
	if (attr(shapes, "shp.type") != "poly")
		stop("Not polygon shapes")
	if (missing(IDs)) stop("IDs required")
	if (length(IDs) != attr(shapes,'nshps')) 
		stop("Number of shapes and IDs differ")
	tab <- table(factor(IDs))
	n <- length(tab)
	IDss <- names(tab)
	reg <- match(IDs, IDss)
	belongs <- lapply(1:n, function(x) which(x == reg))
# assemble the list of Polygons
	Srl <- vector(mode="list", length=n)
	for (i in 1:n) {
		nParts <- length(belongs[[i]])
		srl <- NULL
		for (j in 1:nParts) {
			jres <- .shp2srs(shapes[[belongs[[i]][j]]], 
				nParts.shp(shapes[[belongs[[i]][j]]]))
			srl <- c(srl, jres)
		}
		Srl[[i]] <- Polygons(srl, ID=IDss[i])
	}
	res <- as.SpatialPolygons.PolygonsList(Srl, proj4string=proj4string)
	res
}

# to be moved to glue with maps:

as.SpatialPolygons.map <- function(map, IDs, proj4string=CRS(as.character(NA))) {
	if (missing(IDs)) stop("IDs required")
	xyList <- .NAmat2xyList(cbind(map$x, map$y))
	if (length(xyList) != length(IDs)) stop("map and IDs differ in length")
	tab <- table(factor(IDs))
	n <- length(tab)
	IDss <- names(tab)
	reg <- match(IDs, IDss)
	belongs <- lapply(1:n, function(x) which(x == reg))
# assemble the list of Polygons
	Srl <- vector(mode="list", length=n)
	for (i in 1:n) {
		nParts <- length(belongs[[i]])
		srl <- vector(mode="list", length=nParts)
		for (j in 1:nParts) {
			srl[[j]] <- Polygon(coords=xyList[[belongs[[i]][j]]])
		}
		Srl[[i]] <- Polygons(srl, ID=IDss[i])
	}
	res <- as.SpatialPolygons.PolygonsList(Srl, proj4string=proj4string)
	res
}

# to be moved to glue with RarcInfo:

as.SpatialPolygons.pal <- function(arc, pal, IDs, dropPoly1=TRUE, 
	proj4string=CRS(as.character(NA))) {
	if (missing(IDs)) stop("IDs required")
	if (dropPoly1) pale <- lapply(pal[[2]][-1], function(x) x[[1]])
	else pale <- lapply(pal[[2]], function(x) x[[1]])
	if (length(pale) != length(IDs)) stop("map and IDs differ in length")
	tab <- table(factor(IDs))
	n <- length(tab)
	IDss <- names(tab)
	reg <- match(IDs, IDss)
	belongs <- lapply(1:n, function(x) which(x == reg))
# assemble the list of Polygons
	Srl <- vector(mode="list", length=n)
	for (i in 1:n) {
		bi <- belongs[[i]]
		nParts <- length(bi)
		palei_list <- list()
		for (j in 1:nParts) {
			this <- bi[j]
			paleij <- pale[[this]]
			if (any(paleij == 0)) {
				zeros <- which(paleij == 0)
				palei_list <- c(palei_list, 
					list(paleij[1:(zeros[1]-1)]))
				for (k in 1:length(zeros)) {
					if (k == length(zeros)) {
						lp <- length(paleij)
						lz <- zeros[length(zeros)]
						palei_list <- c(palei_list, 
						    list(paleij[(lz+1):lp]))
					} else {
						zk <- zeros[k]
						zk1 <- zeros[k+1]
						palei_list <- c(palei_list, 
						    list(paleij[(zk+1):(zk1-1)]))
					}
				}
			} else palei_list <- c(palei_list, list(paleij))
		}
		nParts <- length(palei_list)
		srl <- vector(mode="list", length=nParts)
		for (j in 1:nParts) {
			paleij <- palei_list[[j]]
			nArcs <- length(paleij)
			x <- NULL
			y <- NULL
			for (k in 1:nArcs) {
				kk <- paleij[k]
				if (kk > 0) {
					x <- c(x, arc[[2]][[kk]][[1]])
					y <- c(y, arc[[2]][[kk]][[2]])
				} else {
					x <- c(x, rev(arc[[2]][[-kk]][[1]]))
					y <- c(y, rev(arc[[2]][[-kk]][[2]]))
				}
			}
			if ((x[1] != x[length(x)]) || (y[1] != y[length(y)])) {
				x <- c(x, x[1])
				y <- c(y, y[1])
			}
			srl[[j]] <- Polygon(coords=cbind(x, y))	
		}
		Srl[[i]] <- Polygons(srl, ID=IDss[i])
	}
	res <- as.SpatialPolygons.PolygonsList(Srl, proj4string=proj4string)
	res
}


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
	if (!is.matrix(coords)) stop("coords must be a two-column matrix")
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

	nParts <- length(srl)
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
	if (!missing(j)) stop("only a single index is allowed for [.SpatialPolygons")
	if (is.logical(i)) {
		if (length(i) == 1 && i)
			i = 1:length(x@polygons)
		else
			i <- which(i)
	}
	if (length(unique(i)) != length(i))
		stop("SpatialPolygons selection: can't find plot order if polygons are replicated")
	SpatialPolygons(x@polygons[i], pO = order(match(i, x@plotOrder)),
		CRS(proj4string(x)))
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

setMethod("summary", "SpatialPolygons", summary.Spatial)
