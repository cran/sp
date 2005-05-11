if (!isGeneric("recenter"))
	setGeneric("recenter", function(obj)
		standardGeneric("recenter"))

recenter.SpatialRings <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	pls <- getSRpolygonsSlot(obj)
	Srl <- lapply(pls, recenter)
	res <- as.SpatialRings.SringsList(Srl)
	res
}

setMethod("recenter", "SpatialRings", recenter.SpatialRings)

recenter.Srings <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	ID <- getSringsIDSlot(obj)
	rings <- getSringsSringsSlot(obj)
	srl <- lapply(rings, recenter)
	res <- Srings(srl, ID=ID)
	res
}

setMethod("recenter", "Srings", recenter.Srings)

recenter.Sring <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	projargs <- CRS(proj4string(obj))
	crds <- getSringCoordsSlot(obj)
	hole <- getSringHoleSlot(obj)
	inout <- (crds[,1] < 0)
	if (all(inout)) {
		crds[,1] <- crds[,1]+360
	} else {
		if (any(inout)) {
			crds[,1] <- ifelse(inout, crds[,1]+360, crds[,1])
		}
	}
	res <- Sring(crds, projargs, hole)
	res
}

setMethod("recenter", "Sring", recenter.Sring)


recenter.SpatialLines <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	lns <- getSLlinesSlot(obj)
	Sll <- lapply(lns, recenter)
	res <- SpatialLines(Sll)
	res
}

setMethod("recenter", "SpatialLines", recenter.SpatialLines)


recenter.Slines <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
#	ID <- getSlinesIDSlot(obj)
	lines <- getSlinesSlinesSlot(obj)
	sll <- lapply(lines, recenter)
#	res <- Slines(sll, ID=ID)
	res <- Slines(sll)
	res
}

setMethod("recenter", "Slines", recenter.Slines)

recenter.Sline <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	projargs <- CRS(proj4string(obj))
	crds <- coordinates(obj)
	inout <- (crds[,1] < 0)
	if (all(inout)) {
		crds[,1] <- crds[,1]+360
	} else {
		if (any(inout)) {
			crds[,1] <- ifelse(inout, crds[,1]+360, crds[,1])
		}
	}
	res <- Sline(crds, projargs)
	res
}

setMethod("recenter", "Sline", recenter.Sline)

nowrapSpatialLines <- function(obj, offset=0, eps=rep(.Machine$double.eps, 2)) {
	if (!is(obj, "SpatialLines")) stop("obj not a SpatialLines object")
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	bblong <- bbox(obj)[1,]
	inout <- bblong[1] < offset && bblong[2] >= offset
	if (inout) {
		pls <- getSLlinesSlot(obj)
		Srl <- lapply(pls, .nowrapSlines, offset=offset, eps=eps)
		res <- SpatialLines(Srl)
	} else res <- obj
	res
}

.nowrapSlines <- function(obj, offset=0, eps=rep(.Machine$double.eps, 2)) {
	bbo <- bbox(obj)
	inout <- bbo[1,1] < offset && bbo[1,2] >= offset
	if (inout) {
#		ID <- getSlinesIDSlot(obj)
		lines <- getSlinesSlinesSlot(obj)
		sll <- list()
		for (i in 1:length(lines)) {
			sll <- c(sll, .nowrapSline(lines[[i]], 
				offset=offset, eps=eps))
		}
#		res <- Slines(sll, ID=ID)
		res <- Slines(sll)
	} else res <- obj
	res
}

.nowrapSline <- function(obj, offset=0, eps=rep(.Machine$double.eps, 2)) {
	bbo <- bbox(obj)
	inout <- bbo[1,1] < offset && bbo[1,2] >= offset
	if (inout) {
		proj4CRS <- CRS(proj4string(obj))
		crds <- coordinates(obj)
		zoffset <- as.logical(sapply(crds[,1], function(x) 
			all.equal(x, offset, tolerance = .Machine$double.eps)))
		if (any(zoffset)) {
			brks <- which(zoffset)
			n <- length(brks)
			for (i in 1:n) {
				ib <- brks[i]
				if (crds[(ib-1),1] < offset) 
					crds[ib,1] <- crds[ib,1] - eps[1]
				else crds[ib,1] <- crds[ib,1] + eps[1]
			}
		}
		inout <- (crds[,1] <= offset)
		rle_io <- rle(as.integer(inout))
		brks <- cumsum(rle_io$lengths)
		n <- length(brks)
		res <- list()
		if (n == 0) {
			if (all(crds[,1] < offset)) 
				crds[,1] <- crds[,1] - eps[1]
			else crds[,1] <- crds[,1] + eps[2]
			res <- c(res, list(Sline(crds, proj4CRS)))
		} else {
			for (i in 1:n) {
				if (i == n) outcrds <- crds
				else {
					ib <- brks[i]
					pt1 <- crds[ib,]
					pt2 <- crds[(ib+1),]
					dpts <- pt1 - pt2
					if (pt1[1] <= offset) {
						x1 <- offset - eps[1]
						x2 <- offset + eps[2]
						y1 <- pt1[2] + 
							abs(pt1[1]-eps[1])*
							(dpts[2] / dpts[1])
						y2 <- pt1[2] + 
							abs(pt1[1]+eps[2])*
							(dpts[2] / dpts[1])
					} else {
						x1 <- offset + eps[1]
						x2 <- offset - eps[2]
						y1 <- pt1[2] - 
							abs(pt1[1]-eps[1])*
							(dpts[2] / dpts[1])
						y2 <- pt1[2] - 
							abs(pt1[1]+eps[2])*
							(dpts[2] / dpts[1])
					}
					outcrds <- rbind(crds[1:ib,], c(x1, y1))
					brks <- brks - nrow(outcrds) + 2
					crds <- rbind(c(x2, y2), crds[-(1:ib),])
				}
				res <- c(res, list(Sline(outcrds, proj4CRS)))
			}
		}
	} else res <- list(obj)
	res
}

