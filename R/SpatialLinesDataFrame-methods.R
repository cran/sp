SpatialLinesDataFrame = function(sl, data) {
	new("SpatialLinesDataFrame", sl, data = data)
}

plot.SpatialLinesDataFrame = function(x, ...) plotSpatialLines(x, ...)

summary.SpatialLinesDataFrame = summary.Spatial

names.SpatialLinesDataFrame = function(x) names(x@data)

as.data.frame.SpatialLinesDataFrame = function(x, row.names, optional) x@data

setAs("SpatialLinesDataFrame", "data.frame", function(from)
    as.data.frame.SpatialLinesDataFrame(from))

#"[.SpatialLinesDataFrame" <- function(x, i, j, ... , drop = FALSE) {
setMethod("[", "SpatialLinesDataFrame", function(x, i, j, ... , drop = FALSE) {
    missing.i = missing(i)
    missing.j = missing(j)
    if (drop)
        stop("coerce to data.frame first for drop = TRUE")
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
        stop("matrix argument not supported in SpatialRingsDataFrame selection")
    SpatialLinesDataFrame(as(x, "SpatialLines")[i, , drop = FALSE],
        data = x@data[i, j, drop = FALSE])
})

"[[.SpatialLinesDataFrame" =  function(x, ...)
#setMethod("[[", "SpatialRingsDataFrame", function(x, ...)
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
