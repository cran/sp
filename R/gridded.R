"gridded<-" = function(obj, value) {
	if (is(obj, "Spatial"))
		p4s = proj4string(obj)
	else
		p4s = as.character(NA)
	if (is.logical(value)) {
		if (is(obj, "SpatialPixelsDataFrame") || is(obj, "SpatialGridDataFrame")) {
			if (!value)
				obj = as(obj, "SpatialPointsDataFrame")
		} else if (is(obj, "SpatialPixels")) {
			if (!value)
				obj = as(obj, "SpatialPoints")
		} else if (is(obj, "SpatialPointsDataFrame")) {
			if (value)
				obj = SpatialPixelsDataFrame(obj, obj@data)
		} else if (is(obj, "SpatialPoints")) {
			if (value)
				#obj = as(obj, "SpatialGrid")
				obj = SpatialPixels(obj)
		} else
			stop("gridded<- only works for SpatialPoints[DataFrame] or SpatialGrid[DataFrame]")
	} else {
		if (is(value, "formula") || is(value, "character")) {
			if (!is(obj, "data.frame"))
				stop("data.frame expected as object")
			coordinates(obj) = value
			gridded(obj) = TRUE
		} else if (is(value, "GridTopology"))
			return(SpatialGridDataFrame(grid = SpatialGrid(grid = value), data.frame(obj)))
		else
			stop(paste("cannot deal with value of class"), class(value))
		# further deal with more complex forms of value
	}
	proj4string(obj) = CRS(p4s)
	obj
}

setMethod("gridded", "Spatial", function(obj) is(obj, "SpatialPixels"))

fullgrid = function(obj) return(is(obj, "SpatialGrid"))

"fullgrid<-" = function(obj, value) {
	p4s = proj4string(obj)
	if (!is(obj, "SpatialPixels"))
		stop("fullgrid<- only works on objects of class or extending SpatialPixels")
	if (fullgrid(obj) != value) { # convert:
		if (value) { # convert to full grid
			if (is(obj, "SpatialPixelsDataFrame"))
				obj = as(obj, "SpatialGridDataFrame")
			else
				obj = as(obj, "SpatialGrid")
		} else { # convert to Pixels
			if (is(obj, "SpatialGridDataFrame"))
				obj = as(obj, "SpatialPixelsDataFrame")
			else
				obj = as(obj, "SpatialPixels")
		}
	}
	proj4string(obj) = CRS(p4s)
	obj
}
