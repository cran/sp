setClass("SpatialPixelsDataFrame",
	representation("SpatialPixels", data = "data.frame", coords.nrs = "numeric"),
	contains = "SpatialPointsDataFrame",
	validity = function(object) {
		if (length(object@grid.index) != nrow(object@data))
			stop("grid.index should have length equal to data slot")
		if (nrow(object@coords) != nrow(object@data))
			stop("unequal number of objects in points and data.frame")
		return(TRUE)
	}
)

setClass("SpatialGridDataFrame",
	representation("SpatialGrid", data = "data.frame"),
	validity = function(object) {
		if (.NumberOfCells(object@grid) != nrow(object@data))
			stop("unequal number of objects in full grid and data slot")
		return(TRUE)
	}
)
