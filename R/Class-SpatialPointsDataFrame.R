setClass("SpatialPointsDataFrame",
	representation("SpatialPoints", data = "data.frame", coords.nrs = "numeric"),
	prototype = list(new("SpatialPoints"), data = data.frame(), 
		coords.nrs = numeric(0)),
	validity = function(object) {
		if (!inherits(object@data, "data.frame"))
			stop("data should be of class data.frame")
		if (nrow(object@coords) < 1)
			stop("no points set: too few rows")
		if (ncol(object@coords) <= 1)
			stop("no points set: too few columns")
		if (ncol(object@data) == 0)
			stop("data.frame is empty (possibly after stripping coordinate columns): use SpatialPoints() to create points-only object")
		if (nrow(object@data) != nrow(object@coords))
			stop("number of rows in data.frame and SpatialPoints don't match")
		n = length(object@coords.nrs)
		if (n > 0 && n != ncol(object@coords))
			stop("inconsistent coords.nrs slot")
		return(TRUE)
	}
)
