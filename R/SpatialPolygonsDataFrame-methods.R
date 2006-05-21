SpatialPolygonsDataFrame <- function(Sr, data, match.ID = TRUE) {
	if (match.ID) {
		Sr_IDs <- getSpPPolygonsIDSlots(Sr)
		data_IDs <- row.names(data)
		mtch <- match(Sr_IDs, data_IDs)
		if (any(is.na(mtch)))
			stop("row.names of data and Polygons IDs do not match")
		if (length(unique(mtch)) != length(Sr_IDs))
			stop("row.names of data and Polygons IDs do not match")
		data <- data[mtch, , drop = FALSE]
	}
	new("SpatialPolygonsDataFrame", Sr, data=data)
}

setReplaceMethod("polygons", signature(object = "data.frame", value = "SpatialPolygons"),
	function(object, value) SpatialPolygonsDataFrame(value, object))

setMethod("polygons", signature(obj = "SpatialPolygons"),
	function(obj) as(obj, "SpatialPolygons"))

names.SpatialPolygonsDataFrame = function(x) names(x@data)
"names<-.SpatialPolygonsDataFrame" = function(x,value) { names(x@data) = value; x }

as.data.frame.SpatialPolygonsDataFrame = function(x, row.names, optional, ...) x@data

setAs("SpatialPolygonsDataFrame", "data.frame", function(from)
    as.data.frame.SpatialPolygonsDataFrame(from))

setMethod("[", "SpatialPolygonsDataFrame", function(x, i, j, ... , drop = TRUE) {
    missing.i = missing(i)
    missing.j = missing(j)
    drop <- FALSE
#    if (drop)
#       stop("coerce to data.frame first for drop = TRUE")
    nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
    if (missing.i && missing.j) {
        i = TRUE
        j = TRUE
    } else if (missing.j && !missing.i) {
        if (nargs == 2) {
            j = i
            i = TRUE 
        } else {
            j = TRUE
        }
    } else if (missing.i && !missing.j)
        i = TRUE 
    if (is.matrix(i))
        stop("matrix argument not supported in SpatialPolygonsDataFrame selection")
    SpatialPolygonsDataFrame(as(x, "SpatialPolygons")[i, , drop = FALSE],
        data = x@data[i, j, drop = FALSE])
###
### RSB: do something with labelpoints here? How can I check they are present?
### (label points belong to the Polygons objects, not the SpatialPolygons object)
})

"[[.SpatialPolygonsDataFrame" =  function(x, ...)
#setMethod("[[", "SpatialPolygonsDataFrame", function(x, ...)
    x@data[[...]]
#)

"[[<-.SpatialPolygonsDataFrame" =  function(x, i, j, value) {
    if (!missing(j))
        stop("only valid calls are x[[i]] <- value")
    x@data[[i]] <- value
    x
}
"$.SpatialPolygonsDataFrame" = function(x,name) { x@data[[name]] }
"$<-.SpatialPolygonsDataFrame" = function(x,i,value) { x@data[[i]]=value; x }

setMethod("summary", "SpatialPolygonsDataFrame", summary.Spatial)

setMethod("coordinates", "SpatialPolygonsDataFrame", 
	function(obj) getSpPPolygonsLabptSlots(obj))

