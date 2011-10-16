dosth0 = function(obj) {
	cl1 = class(obj)
	if (is(obj, "Spatial"))
		proj4string(obj) = proj4string(obj)
	cl2 = class(obj)
	stopifnot(identical(cl1, cl2))
	obj
}

setGeneric("doNothing<-", function(object, value)
	standardGeneric("doNothing<-"))

xxx = function(object, value) { object }

setReplaceMethod("doNothing", signature(object = "Spatial", value = "ANY"), 
	function(object, value) xxx(object, value))

# if we enable the following, grid.R will not run into an error.
#setReplaceMethod("doNothing", signature(object = "SpatialGrid", value = "ANY"),
#	function(object, value) object
#)
dosth = function(obj) { 
	cl1 = class(obj)
	doNothing(obj) = TRUE
	cl2 = class(obj)
	if (!identical(cl1, cl2)) {
		print(paste(cl1, cl2))
		stopifnot(identical(cl1, cl2))
	}
	obj
}

setMethod("gridded", "Spatial", 
	function(obj) { is(obj, "SpatialPixels") || is(obj, "SpatialGrid") })

# grid -> points:
setReplaceMethod("gridded", c("SpatialGridDataFrame", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPointsDataFrame"); dosth(obj) })
setReplaceMethod("gridded", c("SpatialPixelsDataFrame", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPointsDataFrame"); dosth(obj) })
setReplaceMethod("gridded", c("SpatialGrid", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPoints"); dosth(obj) })
setReplaceMethod("gridded", c("SpatialPixels", "logical"), 
	function(obj, value) { if (!value) obj = as(obj, "SpatialPoints"); dosth(obj) })

# points -> grid:
setReplaceMethod("gridded", c("SpatialPointsDataFrame", "logical"), 
	function(obj, value) { if (value) obj = as(obj, "SpatialPixelsDataFrame"); dosth(obj) })
setReplaceMethod("gridded", c("SpatialPoints", "logical"), 
	function(obj, value) { if (value) obj = as(obj, "SpatialPixels"); dosth(obj) })

# points -> grid, with grid specified as list(value, grid):
setReplaceMethod("gridded", c("SpatialPointsDataFrame", "list"), 
	function(obj, value) { if (value[[1]]) obj = SpatialPixelsDataFrame(obj, obj@data, grid = value[[2]]); dosth(obj) })
setReplaceMethod("gridded", c("SpatialPoints", "list"), 
	function(obj, value) { if (value[[1]]) obj = SpatialPixels(obj, grid = value[[2]]); dosth(obj) })

# data.frame -> gridded:
setReplaceMethod("gridded", c("data.frame", "formula"),
	function(obj, value) { coordinates(obj) = value; gridded(obj) = TRUE; dosth(obj) })
setReplaceMethod("gridded", c("data.frame", "character"),
	function(obj, value) { coordinates(obj) = value; gridded(obj) = TRUE; dosth(obj) })

# data.frame -> gridded, grid specified:
setReplaceMethod("gridded", c("data.frame", "GridTopology"),
	function(obj, value) SpatialGridDataFrame(grid = SpatialGrid(grid = value), data.frame(obj)))

setAs("SpatialPoints", "SpatialPixels", function(from) {
	SpatialPixels(from, grid = NULL)
})

setAs("SpatialPointsDataFrame", "SpatialPixelsDataFrame", function(from) {
	SpatialPixelsDataFrame(from, from@data, grid = NULL)
})

setMethod("fullgrid", c("Spatial"), function(obj) is(obj, "SpatialGrid"))

setReplaceMethod("fullgrid", c("SpatialPixels", "logical"), 
	function(obj, value) { if(value) obj = as(obj, "SpatialGrid"); dosth(obj) })
setReplaceMethod("fullgrid", c("SpatialGrid", "logical"), 
	function(obj, value) { if(!value) obj = as(obj, "SpatialPixels"); dosth(obj) })
setReplaceMethod("fullgrid", c("SpatialPixelsDataFrame", "logical"), 
	function(obj, value) { if(value) obj = as(obj, "SpatialGridDataFrame"); dosth(obj) })
setReplaceMethod("fullgrid", c("SpatialGridDataFrame", "logical"), 
	function(obj, value) { if(!value) obj = as(obj, "SpatialPixelsDataFrame"); dosth(obj) })
#setReplaceMethod("fullgrid", c("Spatial", "ANY"), 
#	function(obj, value) 
#		stop("fullgrid<- only works on objects of class or extending SpatialPixels or SpatialGrid"))
