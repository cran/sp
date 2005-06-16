SpatialRingsDataFrame <- function(Sr, data, match.ID = TRUE) {
	if (match.ID) {
		Sr_IDs <- getSRSringsIDSlots(Sr)
		data_IDs <- row.names(data)
		mtch <- match(Sr_IDs, data_IDs)
		if (any(is.na(mtch)))
			stop("row.names of data and Srings IDs do not match")
		if (length(unique(mtch)) != length(Sr_IDs))
			stop("row.names of data and Srings IDs do not match")
		data <- data[mtch, , drop = FALSE]
	}
	new("SpatialRingsDataFrame", Sr, data=data)
}

"rings<-" = function(object, value) {
	if (!is(value, "SpatialRings"))
		stop("right-hand side should be of class SpatialRings")
	if (!is(object, "data.frame"))
		stop("assigned value should be of class data.frmae")
	SpatialRingsDataFrame(value, object)
}

#setMethod("rings", "SpatialRingsDataFrame", function(obj) as(obj, "SpatialRings"))

names.SpatialRingsDataFrame = function(x) names(x@data)
"names<-.SpatialRingsDataFrame" = function(x,value) { names(x@data) = value; x }

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
### (label points belong to the Srings objects, not the SpatialRings object)
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
