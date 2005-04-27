mapasp <- function(data) {
	# calculates aspect ratio for levelplot of geographic data,
	# using proportial units (compare eqscplot)
	if (!is(data, "Spatial"))
		stop("cannot extract coordinates bounding box from data")
# in R >= 2.0.0, lattice accepts "iso":
#ifdef R
	if (version$major >= 2)
		return("iso")
#endif
	bb = bbox(data)
	diff(bb[2,])/diff(bb[1,])
}
