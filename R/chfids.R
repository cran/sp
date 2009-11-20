chFIDsSpatialLines <- function(obj, x) {
    nl <- length(slot(obj, "lines"))
    if (length(x) != nl) stop("lengths differ")
    if (length(x) > length(unique(x))) stop("duplicate IDs")
    for (i in 1:nl) slot(slot(obj, "lines")[[i]], "ID") <- x[i]
    obj
}

setMethod("spChFIDs", signature(obj="SpatialLines", x="character"),
    chFIDsSpatialLines)

chFIDsSpatialLinesDataFrame <- function(obj, x) {
    SL <- as(obj, "SpatialLines")
    SLx <- spChFIDs(SL, x)
    df <- as(obj, "data.frame")
    row.names(df) <- sapply(slot(SLx, "lines"), function(x) slot(x, "ID"))
    SpatialLinesDataFrame(SLx, data=df)
}

setMethod("spChFIDs", signature(obj="SpatialLinesDataFrame", x="character"),
    chFIDsSpatialLinesDataFrame)

chFIDsSpatialPolygons <- function(obj, x) {
    np <- length(slot(obj, "polygons"))
    if (length(x) != np) stop("lengths differ")
    if (length(x) > length(unique(x))) stop("duplicate IDs")
    for (i in 1:np) slot(slot(obj, "polygons")[[i]], "ID") <- x[i]
    obj
}

setMethod("spChFIDs", signature(obj="SpatialPolygons", x="character"),
    chFIDsSpatialPolygons)

chFIDsSpatialPolygonsDataFrame <- function(obj, x) {
    SP <- as(obj, "SpatialPolygons")
    SPx <- spChFIDs(SP, x)
    df <- as(obj, "data.frame")
    row.names(df) <- sapply(slot(SPx, "polygons"), function(x) slot(x, "ID"))
    SpatialPolygonsDataFrame(SPx, data=df)
}

setMethod("spChFIDs", signature(obj="SpatialPolygonsDataFrame", x="character"),
    chFIDsSpatialPolygonsDataFrame)
