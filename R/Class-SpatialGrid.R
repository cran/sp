setClass("SpatialPixels", 
	representation("SpatialPoints", grid = "GridTopology", grid.index = "integer"),
	validity = function(object) {
# print("Entering validation: SpatialPixels")
		# check that dimensions, proj4string and bbox do not conflict
		if (nrow(object@coords) != length(object@grid.index) 
				&& length(object@grid.index) != 0)
			stop("grid.index should have length zero or equal to nrow(coords)")
		return(TRUE)
	}
)

setClass("SpatialGrid",
	representation("SpatialPixels"),
	validity = function(object) {
# print("Entering validation: SpatialGrid")
		# check that dimensions, proj4string and bbox do not conflict
		if (length(object@grid.index) > 0)
			stop("grid.index should have length zero")
		if (nrow(object@coords) != 2)
			stop("bogus coords should have two rows")
		return(TRUE)
	}
)
