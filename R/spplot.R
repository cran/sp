sp.polygon = function(obj, col = 1, ...) {
	sp.polygon3 = function(x, ...) { 
		cc = getSringCoordsSlot(x)
		grid.polygon(cc[,1], cc[,2], default.units = "native", 
			gp = gpar(col = col, ...))
		panel.lines(cc, col = col, ...)
	}
	if (is.character(obj))
		obj = get(obj)
	if (!is(obj, "SpatialRings"))
		stop(paste("object extending class SpatialRings expected; got class", class(obj)))
	else
		obj = as(obj, "SpatialRings")
	pls = getSRpolygonsSlot(obj)
   	pO <- getSRplotOrderSlot(obj)
	require(grid)
   	for (i in pO) {
   		Srs <- getSringsSringsSlot(pls[[i]])
   		pOi <- getSringsplotOrderSlot(pls[[i]])
   		for (j in pOi)
			sp.polygon3(Srs[[j]], ...)
	}
}

sp.lines = function(obj, col = 1, ...) {
	if (is.character(obj))
		obj = get(obj)
	sp.lines3 = function(x, ...) panel.lines(coordinates(x), col = col, ...)
	sp.lines2 = function(x, ...) lapply(x@Slines, sp.lines3, col = col, ...)
	if (is(obj, "SpatialLines"))
		lapply(obj@lines, sp.lines2, col = col, ...)
	else if (is(obj, "Slines"))
		lapply(obj@Slines, sp.lines3, col = col, ...)
	else if (is(obj, "Sline"))
		panel.lines(coordinates(obj), col = col, ...)
	else stop(paste("obj of class Sline, Slines or SpatialLines expected, got", class(obj)))
}

sp.text = function(loc, txt, ...) {
	if (length(loc) != 2)
		stop("loc should have length 2")
	panel.text(loc[1], loc[2], txt, ...)
}

sp.points = function(obj, pch = 3, ...) {
	if (is.character(obj))
		obj = get(obj)
	xy = coordinates(obj)
	panel.points(xy[,1], xy[,2], pch = pch, ...)
}

sp.panel.layout = function(lst, panel.counter, ...) {
	sp.panel0 = function(x, ...) {
		if (is.character(x))
			obj = get(x)
		if (!is.null(x$which) && is.na(match(panel.counter, x$which)))
			return()
		if (inherits(x, "list")) {
			n = length(x)
			do.call(x[[1]], x[2:n])
		} else if (is(x, "SpatialLines") || is(x, "Slines") || is(x, "Sline"))
			sp.lines(x, ...)
		else if (is(x, "SpatialPoints"))
			sp.points(as(x, "SpatialPoints"), ...)
		else if (is(x, "SpatialRings"))
			sp.polygon(x, ...)
		else stop(paste("cannot plot object of class", class(x)))
	}
	if (!is.null(lst$which) && is.na(match(panel.counter, lst$which)))
		return()
	else
		lst$which = NULL
	if (is.null(lst))
		return()
	if (inherits(lst, "list")) {
		if (inherits(lst[[1]], "list")) 
			lapply(lst, sp.panel0, ...)
		else
			sp.panel0(lst, ...)
	} else
		stop(paste("expected object of class list; got object of class", class(lst)))
}

getFormulaLevelplot = function(sdf, zcol) {
	if (!is.character(zcol)) 
		zcol = names(sdf)[zcol]
	if (length(zcol) > 1)
		as.formula(paste("z~", paste(dimnames(coordinates(sdf))[[2]], 
			collapse = "+"), "|name"))
	else
		as.formula(paste(zcol, "~", paste(dimnames(coordinates(sdf))[[2]],
			collapse = "+")))
}

spplot.grid = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = "", ylab = "", aspect = mapasp(obj), 
		panel = panel.gridplot, sp.layout = NULL) {
	require(lattice)
	sdf = as(obj, "SpatialPointsDataFrame")
	formula = getFormulaLevelplot(sdf, zcol)
	if (length(zcol) > 1)
		sdf = map.to.lev(sdf, zcol = zcol, names.attr = names.attr)
	levelplot(formula, as(sdf, "data.frame"), aspect = aspect,
		panel = panel, xlab = xlab, ylab = ylab, scales = scales,
		sp.layout = sp.layout, ...)
}

setMethod("spplot", signature("SpatialPixelsDataFrame"), spplot.grid)
setMethod("spplot", signature("SpatialGridDataFrame"), 
	function(obj, ...) spplot.grid(as(obj, "SpatialPixelsDataFrame"), ...))

spplot.rings = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = "", ylab = "", aspect = mapasp(obj), 
		panel = panel.ringsplot, sp.layout = NULL) {

	require(lattice)
	require(grid)
	xr = bbox(obj)[1, ] 
	yr = bbox(obj)[2, ]
	sdf = as.data.frame(obj)
	if (is(obj, "SpatialRingsDataFrame"))
		labpts = getSRSringsLabptSlots(obj)
	else {
		# get first points of each lines object:
		n = length(obj@lines)
		labpts = matrix(unlist(lapply(ncl@lines, function(x) 
			lapply(x@Slines[1], function(x) coordinates(x)[1,]))), n, 2, byrow=TRUE) 
	}
	dimnames(labpts)[[2]] = c("xlabelpoint", "ylabelpoint")
	sdf = data.frame(cbind(labpts, sdf))
	coordinates(sdf) = c("xlabelpoint", "ylabelpoint")
	formula = getFormulaLevelplot(sdf, zcol)
	if (length(zcol) > 1)
		sdf = map.to.lev(sdf, zcol = zcol, names.attr = names.attr)
	if (is(obj, "SpatialRingsDataFrame"))
		grid.polygons = as(obj, "SpatialRings")
	else
		grid.polygons = as(obj, "SpatialLines")
	levelplot(formula, as(sdf, "data.frame"), aspect = aspect,
		grid.polygons = grid.polygons,
		panel = panel, xlim = xr, ylim = yr, xlab = xlab, ylab =
		ylab, scales = scales, sp.layout = sp.layout, ...)
}
setMethod("spplot", signature("SpatialRingsDataFrame"), spplot.rings)
setMethod("spplot", signature("SpatialLinesDataFrame"), spplot.rings)

spplot.points = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = "", ylab = "", 
		aspect = mapasp(obj), panel = panel.pointsplot,
		sp.layout = NULL, identify = FALSE) {

	dots = list(...)
	require(lattice)
	sdf = obj
	# create formula:
	if (length(zcol) > 1) {
		formula = as.formula(paste(paste(dimnames(coordinates(sdf))[[2]][2:1], 
			collapse = "~"), "|name"))
		sdf = map.to.lev(sdf, zcol = zcol, names.attr = names.attr)
	} else {
		if (!is.character(zcol)) 
			zcol = names(sdf)[zcol]
		ccn = dimnames(coordinates(sdf))[[2]]
		formula = as.formula(paste(ccn[2], "~", ccn[1]))
	}
	args.xyplot = append(list(formula = formula, data = as(sdf, "data.frame"), 
		panel = panel, aspect = aspect, scales = scales, 
		xlab = xlab, ylab = ylab, sp.layout = sp.layout), dots)
	z = as.vector(as.matrix(as(obj, "data.frame")[zcol]))
	args.xyplot = fill.call.groups(args.xyplot, z = z, ...)
	plt = do.call("xyplot", args.xyplot)
	if (!(is.logical(identify) && identify==FALSE) && interactive()) {
		print(plt)
		if (!(is.numeric(identify) && length(identify) == 2))
			idenfity = c(1,1)
		trellis.focus("panel", identify[1], identify[2])
		labels = row.names(as(sdf, "data.frame"))
		cat("left-mouse to identify points; right-mouse to end\n")
		cc = coordinates(meuse)
		x = cc[,1]
		y = cc[,2]
		ret = panel.identify(x, y, labels)
		trellis.unfocus()
		return(ret)
	} else
		plt
}
setMethod("spplot", signature("SpatialPointsDataFrame"), spplot.points)

panel.gridplot = function(x, y, z, subscripts, ..., panel.counter, sp.layout) {
	panel.levelplot(x, y, z, subscripts, ...)
	sp.panel.layout(sp.layout, panel.counter)
}

panel.ringsplot =
function (x, y, z, subscripts, at = pretty(z), shrink, labels = NULL, 
   		label.style = c("mixed", "flat", "align"), contour = FALSE, 
   		region = TRUE, col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
   		cex = add.text$cex, font = add.text$font, fontfamily = add.text$fontfamily, 
   		fontface = add.text$fontface, col.text = add.text$col, ..., 
   		col.regions = regions$col, alpha.regions = regions$alpha, 
		grid.polygons, sp.layout, panel.counter) 
{
	regions <- trellis.par.get("regions")
	add.line <- trellis.par.get("add.line")
	numcol <- length(at) - 1
	numcol.r <- length(col.regions)
	col.regions <- if (numcol.r <= numcol) 
   			rep(col.regions, length = numcol)
   		else col.regions[floor(1 + (1:numcol - 1) * (numcol.r - 1)/(numcol - 1))]
	zcol <- rep(NA, length(z))
	for (i in seq(along = col.regions)) zcol[!is.na(x) & !is.na(y) & 
      			!is.na(z) & z >= at[i] & z < at[i + 1]] <- i
	label.style <- match.arg(label.style)
	x <- as.numeric(x[subscripts])
	y <- as.numeric(y[subscripts])
	z <- as.numeric(z[subscripts])
	zcol <- as.numeric(zcol[subscripts])
	if (any(subscripts)) {
		if (is(grid.polygons, "SpatialLines")) {
			sp.lines3 = function(x, col, ...) panel.lines(coordinates(x), col = col, ...)
			sp.lines2 = function(x, col, ...) lapply(x@Slines, sp.lines3, col, ...)
			for (i in 1:length(grid.polygons@lines))
				sp.lines2(grid.polygons@lines[[i]], col = col.regions[zcol[i]], ...)
		} else {
			pls = getSRpolygonsSlot(grid.polygons)
   			pO = getSRplotOrderSlot(grid.polygons)
   			for (i in pO) {
       			Srs <- getSringsSringsSlot(pls[[i]])
       			pOi <- getSringsplotOrderSlot(pls[[i]])
       			for (j in pOi) {
					coords = getSringCoordsSlot(Srs[[j]])
					if (getSringHoleSlot(Srs[[j]])) {
						bg = trellis.par.get()$background
						if (bg$col == "transparent")
							fill = "white"
						else
							fill = bg$col
						alpha = bg$alpha
					} else {
						fill = col.regions[zcol[i]]
						alpha = alpha.regions
					}
					gp = gpar(fill = fill, alpha = alpha, col = col)
					grid.polygon(coords[,1], coords[,2], default.units = "native", 
						gp = gp)
				}
   			}
		}
	}
	sp.panel.layout(sp.layout, panel.counter)
}

panel.pointsplot = function(x, y, subscripts, col, sp.layout, ..., panel.counter) {
	sp.panel.layout(sp.layout, panel.counter)
	panel.superpose(x, y, subscripts, col = col, ...)
}

fill.call.groups = function(lst, z, ..., cuts, col.regions = trellis.par.get("regions")$col, 
		legendEntries = "", pch, cex = 1, fill = TRUE, do.log = FALSE, key.space = "bottom") {
    if (missing(pch)) 
        lst$pch = ifelse(fill, 16, 1)
	if (missing(cuts))
		cuts = 5
	if (length(cuts) > 1)
		ncuts = length(cuts) - 1
	else {
		if (is.numeric(z))
			ncuts = cuts
		else
			ncuts = length(unique(z))
	}
	if (ncuts != length(col.regions)) {
		cols = round(1+(length(col.regions)-1)*(0:(ncuts-1))/(ncuts-1))
		lst$col = col.regions[cols]
	} else
		lst$col = col.regions
	if (!missing(cex))
		lst$cex = cex
	if (is.numeric(z)) {
    	if (length(cuts) == 1) {
			if (do.log) {
       			lz = log(z)
       			cuts = c(min(z), exp(seq(min(lz), max(lz), length = cuts + 
           			1))[2:(cuts)], max(z))
			} else
				cuts = seq(min(z), max(z), length = cuts + 1)
    	}
    	lst$groups = cut(as.matrix(z), cuts, dig.lab = 4, include.lowest = TRUE)
	} else
		lst$groups = factor(z)
	if (missing(legendEntries))
		legendEntries = levels(lst$groups)
	n = length(levels(lst$groups))
	lst$key = list(points = list(pch = rep(lst$pch, 
		length = n), col = rep(lst$col, length = n), 
		cex = rep(cex, length = n)), text = list(legendEntries))
	if (is.character(key.space))
		lst$key$space = key.space
	else if (is.list(key.space))
		lst$key = append(lst$key, key.space)
	else
		warning("key.space argument ignored (not list or character)")
	return(lst)
}

SpatialRings2Grob = function(obj, fill) {
	if (!is(obj, "SpatialRings"))
		stop("object is not of class SpatialRings")
	x = numeric(0)
	y = numeric(0)
	id = integer(0)
	pls = getSRpolygonsSlot(obj)
   	pO <- getSRplotOrderSlot(obj)
	n = 0
   	for (i in pO) {
   		Srs <- getSringsSringsSlot(pls[[i]])
   		pOi <- getSringsplotOrderSlot(pls[[i]])
   		for (j in pOi) {
			n = n + 1
			cc = getSringCoordsSlot(Srs[[j]])
			x = c(x, cc[,1])
			y = c(y, cc[,2])
			id = c(id, rep(n, nrow(cc)))
		}
	}
	require(grid)
	polygonGrob(x=x, y=y, id=id, gp = gpar(fill = fill))
}

SpatialRingsRescale = function(obj, offset, scale = 1, fill = "black", col = "black",...) {
	require(grid)
	if (!is(obj, "SpatialRings"))
		stop("object is not of class SpatialRings")
	if (length(offset) != 2)
		stop("offset should have length 2")
	if (length(scale) == 1)
		scale = rep(scale,2)
	pls = getSRpolygonsSlot(obj)
   	pO = getSRplotOrderSlot(obj)
	fill = rep(fill, length = length(pls))
   	for (i in pO) {
   		Srs <- getSringsSringsSlot(pls[[i]])
   		pOi <- getSringsplotOrderSlot(pls[[i]])
   		for (j in pOi) {
			cc = getSringCoordsSlot(Srs[[j]])
			x = offset[1] + (cc[,1] * scale[1])
			y = offset[2] + (cc[,2] * scale[2])
			grid.polygon(x, y, default.units = "native", 
				gp = gpar(col = col, fill = fill[i], ...))
		}
	}
}

mapLegendGrob <- function(obj, widths = unit(1, "cm"), heights = unit(1, "cm"),
		fill = "black", just = "right") {
	grb = SpatialRings2Grob(obj, fill)
	key.layout <- grid.layout(nrow = 1, ncol = 1, widths = widths,
					heights = heights, respect = TRUE, just = just)
	key.gf <- frameGrob(layout = key.layout)
	key.gf <- placeGrob(key.gf,
				  rectGrob(gp = gpar(fill = "transparent", col = NULL)),
				  row = NULL, col = NULL)
	key.gf <- placeGrob(key.gf, grb, row = 1, col = 1)
	key.gf
}

layout.north.arrow = function() {
	x1 = c(0.1653, 0.2241, 0.2241, 0.2830, 0.1947, 0.1065, 0.1653, 0.1653)
	x2 = c(0, 0.0967, 0.0967, 0.2928, 0.3908, 0.3908, 0.2928, 0.2928, 0.1032, 0, 0)
	y1 = c(0, 0, 0.8823, 0.8235, 1, 0.8235, 0.8823, 0)
	y2 = c(0.2352, 0.2352, 0.5686, 0.2352, 0.2352, 0.7189, 0.7189, 0.3986, 0.7189, 0.7189, 0.2352 )
	SpatialRings(list(Srings(list(Sring(cbind(x1,y1)), Sring(cbind(rev(x2),rev(y2)))), ID="north")))
}
# north.arrow = .north.arrow()

layout.scale.bar = function(height = 0.05) {
	x1 = c(0, 0.5, 0.5, 0, 0)
	y1 = c(0, 0, height, height, 0)
	x2 = x1 + 0.5
	y2 = y1
	SpatialRings(list(Srings(list(Sring(cbind(x1,y1))), ID="left"), 
			Srings(list(Sring(cbind(rev(x2),rev(y2)))), ID="right")))
}
# scale.bar = .scale.bar()

sp.theme = function() list(
	regions = list(col = bpy.colors(100))
)

spplot.key = function(sp.layout, rows = 1, cols = 1) {
	for (i in seq(along=rows)) {
		for (j in seq(along=cols)) {
			trellis.focus("panel", cols[j], rows[i], highlight = FALSE)
			sp.panel.layout(sp.layout)
			trellis.unfocus()
		}
	}
}

sp.pagefn = function(n) {
	pos = lattice:::lattice.getStatus("current.panel.positions")
	spplot.key(sp.layout, pos[1], pos[2])
}
