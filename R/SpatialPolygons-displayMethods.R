#
plot.SpatialPolygons <- function(x, col, border = par("fg"), add=FALSE, 
	xlim=NULL, ylim=NULL, xpd = NULL, density = NULL, angle = 45, 
	pbg=NULL, axes = FALSE, ..., setParUsrBB=FALSE) {

	if (is.null(pbg))
		pbg = par("bg") # transparent!
	if (!is(x, "SpatialPolygons")) 
		stop("Not a SpatialPolygons object")

	if (! add) 
		plot(as(x, "Spatial"), xlim=xlim, ylim=ylim, axes = axes, 
			..., setParUsrBB=setParUsrBB)

	n <- length(slot(x, "polygons"))
	if (length(border) != n)
		border <- rep(border, n, n)
	polys <- slot(x, "polygons")
	pO <- slot(x, "plotOrder")
	if (!is.null(density)) {
		if (missing(col)) col <- par("fg")
		if (length(col) != n) col <- rep(col, n, n)
		if (length(density) != n)
			density <- rep(density, n, n)
		if (length(angle) != n)
			angle <- rep(angle, n, n)
		for (j in pO) 
			.polygonRingHoles(polys[[j]], border = border[j], 
			xpd = xpd, density = density[j], angle = angle[j], 
			col = col[j], pbg = pbg, ...) 
	} else {
		if (missing(col)) col <- NA
		if (length(col) != n) col <- rep(col, n, n)
		for (j in pO) 
			.polygonRingHoles(polys[[j]], col=col[j], 
			border=border[j], xpd = xpd, pbg = pbg, ...)
	}
}

setMethod("plot", signature(x = "SpatialPolygons", y = "missing"),
	function(x, y, ...) plot.SpatialPolygons(x, ...))

.polygonRingHoles <- function(Sr, col=NA, border=NULL, xpd=NULL, density=NULL,
	angle=45, pbg, ...) {
	if (!is(Sr, "Polygons")) 
		stop("Not an Polygons object")
	if (!is.null(density)) hatch <- TRUE
	else hatch <- FALSE
	pO <- slot(Sr, "plotOrder")
	polys <- slot(Sr, "Polygons")
	
	for (i in pO) {
		if (hatch) {
			if (!slot(polys[[i]], "hole"))
				.polygon(slot(polys[[i]], "coords"), 
					border = border, xpd = xpd, 
					density = density, angle = angle,
					col=col, hatch=TRUE, ...)
			else .polygon(slot(polys[[i]], "coords"), 
					border = border, xpd = xpd, col=pbg, 
					density = NULL, ...)
		} else {
			if (!slot(polys[[i]], "hole"))
				.polygon(slot(polys[[i]], "coords"), 
					border = border, xpd = xpd, 
					col=col, ...)
			else .polygon(slot(polys[[i]], "coords"), 
				border = border, xpd = xpd, col=pbg, ...)
		}
	}
}


.polygon = function(x, y = NULL, density = NULL, angle = 45,
	border = NULL, col = NA, lty = NULL, xpd = NULL, hatch=NA, ...) {
	if (is.na(hatch)) polygon(x = x, y = y, border = border, 
		col = col, lty = lty, xpd = xpd, ...)
	else polygon(x = x, y = y, density = density, angle = angle, 
		border = border, lty = lty, xpd = xpd, col=col, ...)
}


