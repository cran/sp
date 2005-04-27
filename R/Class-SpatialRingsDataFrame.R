setClass("SpatialRingsDataFrame",
	representation("SpatialRings", data = "data.frame"),
	prototype = list(new("SpatialRings"), data = data.frame()),
	validity = function(object) {
		if (!inherits(object@data, "data.frame"))
			stop("data should be of class data.frame")
		if (nrow(object@data) != length(object@polygons))
		  stop("number of rows in data.frame and polygons in SpatialRings don't match")
		return(TRUE)
	}
)

