SpatialLinesDataFrame = function(sl, data, match.ID = TRUE) {
	if (match.ID) {
		Sl_IDs <- getSLLinesIDSlots(sl)
		data_IDs <- row.names(data)
		mtch <- match(Sl_IDs, data_IDs)
		if (any(is.na(mtch)))
			stop("row.names of data and Lines IDs do not match")
		if (length(unique(mtch)) != length(Sl_IDs))
			stop("row.names of data and Lines IDs do not match")
		data <- data[mtch, , drop=FALSE]
	}
	new("SpatialLinesDataFrame", sl, data = data)
}

setMethod("summary", "SpatialLinesDataFrame", summary.Spatial)

names.SpatialLinesDataFrame = function(x) names(x@data)
"names<-.SpatialLinesDataFrame" = function(x,value) { names(x@data)<-value; x }

as.data.frame.SpatialLinesDataFrame = function(x, row.names, optional, ...) x@data

setAs("SpatialLinesDataFrame", "data.frame", function(from)
    as.data.frame.SpatialLinesDataFrame(from))

#"[.SpatialLinesDataFrame" <- function(x, i, j, ... , drop = FALSE) {
setMethod("[", "SpatialLinesDataFrame", function(x, i, j, ... , drop = TRUE) {
    missing.i = missing(i)
    missing.j = missing(j)
    drop <- FALSE
#    if (drop)
#        stop("coerce to data.frame first for drop = TRUE")
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
        stop("matrix argument not supported in SpatialLinesDataFrame selection")
    if (any(is.na(i))) stop("NAs not permitted in row index")
    SpatialLinesDataFrame(as(x, "SpatialLines")[i, , drop = FALSE],
        data = x@data[i, j, drop = FALSE])
})

"[[.SpatialLinesDataFrame" =  function(x, ...)
#setMethod("[[", "SpatialLinesDataFrame", function(x, ...)
    x@data[[...]]
#)

"[[<-.SpatialLinesDataFrame" =  function(x, i, j, value) {
    if (!missing(j))
        stop("only valid calls are x[[i]] <- value")
    x@data[[i]] <- value
    x
}
"$.SpatialLinesDataFrame" = function(x,name) { x@data[[name]] }
"$<-.SpatialLinesDataFrame" = function(x,i,value) { x@data[[i]]=value; x }
