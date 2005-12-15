setClass("SpatialPixelsDataFrame",
	representation("SpatialPixels", data = "AttributeList", coords.nrs = "numeric"),
	validity = function(object) {
		if (length(object@grid.index) == 0)
			stop("grid.index should not have length zero")
		if (nrow(object@coords) != nrow(object@data))
			stop("unequal number of objects in points and data.frame")
		return(TRUE)
	}
)

setClass("SpatialGridDataFrame",
	representation("SpatialGrid", data = "AttributeList"),
	validity = function(object) {
		if (length(object@grid.index) > 0)
			stop("grid.index should have length zero")
		if (.NumberOfCells(object@grid) != nrow(object@data))
			stop("unequal number of objects in full grid and data slot")
		return(TRUE)
	}
)
