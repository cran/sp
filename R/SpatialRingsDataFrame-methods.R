SpatialRingsDataFrame <- function(Sr, data)
	new("SpatialRingsDataFrame", Sr, data = data)

"rings<-" = function(object, value) {
	if (!is(value, "SpatialRings"))
		stop("right-hand side should be of class SpatialRings")
	if (!is(object, "data.frame"))
		stop("assigned value should be of class data.frmae")
	SpatialRingsDataFrame(value, object)
}

#setMethod("rings", "SpatialRingsDataFrame", function(obj) as(obj, "SpatialRings"))

names.SpatialRingsDataFrame = function(x) names(x@data)

as.data.frame.SpatialRingsDataFrame = function(x, row.names, optional) x@data

setAs("SpatialRingsDataFrame", "data.frame", function(from)
    as.data.frame.SpatialRingsDataFrame(from))

#"[.SpatialRingsDataFrame" <- function(x, i, j, ... , drop = FALSE) {
setMethod("[", "SpatialRingsDataFrame", function(x, i, j, ... , drop = FALSE) {
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
    SpatialRingsDataFrame(as(x, "SpatialRings")[i, , drop = FALSE],
        data = x@data[i, j, drop = FALSE])
###
### RSB: do something with labelpoints here? How can I check they are present?
###
})

"[[.SpatialRingsDataFrame" =  function(x, ...)
#setMethod("[[", "SpatialRingsDataFrame", function(x, ...)
    x@data[[...]]
#)

"[[<-.SpatialRingsDataFrame" =  function(x, i, j, value) {
    if (!missing(j))
        stop("only valid calls are x[[i]] <- value")
    x@data[[i]] <- value
    x
}
"$.SpatialRingsDataFrame" = function(x,name) { x@data[[name]] }
"$<-.SpatialRingsDataFrame" = function(x,i,value) { x@data[[i]]=value; x }

summary.SpatialRingsDataFrame = summary.Spatial
