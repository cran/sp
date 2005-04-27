# Lancaster, Thu Nov  4 14:44:00 GMT 2004, fresh start from icelfloe
setClass("Spatial",
	representation(bbox = "matrix", proj4string = "CRS"),
	prototype = list(bbox = matrix(rep(NA, 6), 3, 2, dimnames = list(NULL, c("min","max"))),
		proj4string = CRS(as.character(NA))), # prototype should not pass validity
	validity = function(object) {
		bb = bbox(object)
		if (!is.matrix(bb))
			return("bbox should be a matrix")
		n = dimensions(object)
		if (n < 2)
			return("spatial.dimension should be 2 or more") 
		if (any(is.na(bb)))
			return("bbox should never contain NA values")
		if (any(bb[,"max"] < bb[,"min"]))
			return("invalid bbox: max < min")
		if (!is(object@proj4string, "CRS"))
			return("proj4string slot should be of class CRS")
		# validate proj4string here? -- no, that's spproj business
		return(TRUE)
	}
)

