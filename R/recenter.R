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
	crds[,1] <- ifelse((crds[,1] < 0), crds[,1]+360, crds[,1])
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
	crds[,1] <- ifelse((crds[,1] < 0), crds[,1]+360, crds[,1])
	res <- Sline(crds, projargs)
	res
}

setMethod("recenter", "Sline", recenter.Sline)

