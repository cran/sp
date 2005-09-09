sp.polygons = function(obj, col = 1, ...) {
	sp.polygon3 = function(x, ...) { 
		cc = getPolygonCoordsSlot(x)
		grid.polygon(cc[,1], cc[,2], default.units = "native", 
			gp = gpar(col = col, ...))
		panel.lines(cc, col = col, ...)
	}
	if (is.character(obj))
		obj = get(obj)
	if (!is(obj, "SpatialPolygons"))
		stop(paste("object extending class SpatialPolygons expected; got class", class(obj)))
	else
		obj = as(obj, "SpatialPolygons")
	pls = getSpPpolygonsSlot(obj)
   	pO <- getSpPplotOrderSlot(obj)
   	for (i in pO) {
   		Srs <- getPolygonsPolygonsSlot(pls[[i]])
   		pOi <- getPolygonsplotOrderSlot(pls[[i]])
   		for (j in pOi)
			sp.polygon3(Srs[[j]], ...)
	}
}

sp.lines = function(obj, col = 1, ...) {
	if (is.character(obj))
		obj = get(obj)
	sp.lines3 = function(x, ...) panel.lines(coordinates(x), col = col, ...)
	sp.lines2 = function(x, ...) lapply(x@Lines, sp.lines3, col = col, ...)
	if (is(obj, "SpatialLines"))
		lapply(obj@lines, sp.lines2, col = col, ...)
	else if (is(obj, "Lines"))
		lapply(obj@Lines, sp.lines3, col = col, ...)
	else if (is(obj, "Line"))
		panel.lines(coordinates(obj), col = col, ...)
	else stop(paste("obj of class Line, Lines or SpatialLines expected, got", class(obj)))
}

sp.points = function(obj, pch = 3, ...) {
	if (is.character(obj))
		obj = get(obj)
	xy = coordinates(obj)
	panel.points(xy[,1], xy[,2], pch = pch, ...)
}

sp.grid = function(obj, col = 1, alpha = 1, ...) {
	if (is.character(obj))
		obj = get(obj)
	xy = coordinates(obj)
	if (length(col) != 1 && length(col) != nrow(xy)) {
		# do something with col
	}
	gt = as(getGridTopology(obj), "data.frame")
	grid.rect(x = xy[,1], y = xy[,2], width = gt$cellsize[1],
		height = gt$cellsize[2], default.units = "native",
		gp = gpar(fill = col, col = NULL, alpha = alpha))
}

sp.text = function(loc, txt, ...) {
	if (length(loc) != 2)
		stop("loc should have length 2")
	panel.text(loc[1], loc[2], txt, ...)
}

sp.panel.layout = function(lst, panel.number, ...) {
	sp.panel0 = function(x, first = FALSE, ...) {
		if (is.character(x))
			obj = get(x)
		if (!is.null(x$which) && is.na(match(panel.number, x$which)))
			return()
		if (inherits(x, "list")) {
			# print(paste(class(x), "first val", first, "first obj", x$first))
			if (!is.null(x$first)) {
				if (x$first == first)
					do.call(x[[1]], x[2:length(x)])
			} else if (!first)
				do.call(x[[1]], x[2:length(x)])
		} else if (is(x, "SpatialLines") || is(x, "Lines") || is(x, "Line"))
			sp.lines(x, ...)
		else if (is(x, "SpatialPoints"))
			sp.points(as(x, "SpatialPoints"), ...)
		else if (is(x, "SpatialPolygons"))
			sp.polygons(x, ...)
		else if (is(x, "SpatialPixels") || is(x, "SpatialGrid"))
			sp.grid(x, ...)
		else stop(paste("cannot plot object of class", class(x)))
	}
	if (!is.null(lst$which) && is.na(match(panel.number, lst$which)))
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
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
		aspect = mapasp(obj), panel = panel.gridplot, sp.layout = NULL, formula, 
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,]) {
	sdf = as(obj, "SpatialPointsDataFrame")
	if (missing(formula))
		formula = getFormulaLevelplot(sdf, zcol)
	if (length(zcol) > 1) {
		sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
		zcol2 = "z"
	} else
		zcol2 = zcol
	scales = longlat.scales(obj, scales, xlim, ylim)
	args = append(list(formula = formula, data = as(sdf, "data.frame"), 
		aspect = aspect, panel = panel, xlab = xlab, ylab = ylab, scales = scales,
		sp.layout = sp.layout, xlim = xlim, ylim = ylim), list(...))
	# deal with factor variables:
	if (all(unlist(lapply(obj@data@att[zcol], is.factor)))) {
		args$data[[zcol2]] = as.numeric(args$data[[zcol2]])
		if (is.null(args$colorkey) || (is.logical(args$colorkey) && args$colorkey)
				|| (is.list(args$colorkey) && is.null(args$colorkey$at) && 
					is.null(args$colorkey$labels))) {
			if (!is.list(args$colorkey))
				args$colorkey = list()
			ck = args$colorkey
			args$colorkey = NULL
			args = append(args, colorkey.factor(sdf[[zcol2]], ck))
		}
	}
	do.call("levelplot", args)
}

setMethod("spplot", signature("SpatialPixelsDataFrame"), spplot.grid)
setMethod("spplot", signature("SpatialGridDataFrame"), 
	function(obj, ...) spplot.grid(as(obj, "SpatialPixelsDataFrame"), ...))

spplot.polygons = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, aspect = mapasp(obj), 
		panel = panel.polygonsplot, sp.layout = NULL, formula, 
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,]) {

	sdf = as.data.frame(obj)
	if (is(obj, "SpatialPolygonsDataFrame"))
		labpts = getSpPPolygonsLabptSlots(obj)
	else {
		# get first points of each lines object:
		n = length(obj@lines)
		labpts = matrix(unlist(lapply(obj@lines, function(x) 
			lapply(x@Lines[1], function(x) coordinates(x)[1,]))), n, 2, byrow=TRUE) 
	}
	dimnames(labpts)[[2]] = c("xlabelpoint", "ylabelpoint")
	sdf = data.frame(cbind(labpts, sdf))
	coordinates(sdf) = c("xlabelpoint", "ylabelpoint")
	if (missing(formula))
		formula = getFormulaLevelplot(sdf, zcol)
	if (length(zcol) > 1) {
		sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
		zcol2 = "z"
	} else
		zcol2 = zcol
	if (is(obj, "SpatialPolygonsDataFrame"))
		grid.polygons = as(obj, "SpatialPolygons")
	else
		grid.polygons = as(obj, "SpatialLines")
	scales = longlat.scales(obj, scales, xlim, ylim)

	args = append(list(formula = formula, data = as(sdf, "data.frame"),
		aspect = aspect, grid.polygons = grid.polygons, panel =
		panel, xlab = xlab, ylab = ylab, scales = scales,
		sp.layout = sp.layout, xlim = xlim, ylim = ylim), list(...))
	if (all(unlist(lapply(obj@data[zcol], is.factor)))) {
		args$data[[zcol2]] = as.numeric(args$data[[zcol2]])
		if (is.null(args$colorkey) || (is.logical(args$colorkey) && args$colorkey)
				|| (is.list(args$colorkey) && is.null(args$colorkey$at) && 
					is.null(args$colorkey$labels))) {
			if (!is.list(args$colorkey))
				args$colorkey = list()
			ck = args$colorkey
			args$colorkey = NULL
			args = append(args, colorkey.factor(sdf[[zcol2]], ck))
		}
	}
	do.call("levelplot", args)

	#levelplot(formula, as(sdf, "data.frame"), aspect = aspect,
	#	grid.polygons = grid.polygons, panel = panel, xlab = xlab, ylab =
	#	ylab, scales = scales, sp.layout = sp.layout, xlim = xlim, ylim =
	#	ylim, ...)
}

setMethod("spplot", signature("SpatialPolygonsDataFrame"), spplot.polygons)
setMethod("spplot", signature("SpatialLinesDataFrame"), spplot.polygons)

spplot.points = function(obj, zcol = names(obj), ..., names.attr, 
		scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
		aspect = mapasp(obj), panel = panel.pointsplot,
		sp.layout = NULL, identify = FALSE, formula,
		xlim = bbexpand(bbox(obj)[1,], 0.04), ylim = bbexpand(bbox(obj)[2,], 0.04)) {

	dots = list(...)
	sdf = obj
	# create formula:
	if (missing(formula)) {
		if (length(zcol) > 1) {
			formula = as.formula(paste(paste(dimnames(coordinates(sdf))[[2]][2:1], 
				collapse = "~"), "|name"))
			sdf = spmap.to.lev(sdf, zcol = zcol, names.attr = names.attr)
		} else {
			if (!is.character(zcol)) 
				zcol = names(sdf)[zcol]
			ccn = dimnames(coordinates(sdf))[[2]]
			formula = as.formula(paste(ccn[2], "~", ccn[1]))
		}
	}
	args.xyplot = append(list(formula = formula, data = as(sdf, "data.frame"), 
		panel = panel, aspect = aspect, scales = scales, 
		xlab = xlab, ylab = ylab, sp.layout = sp.layout,
		xlim = xlim, ylim = ylim), dots)
	z = as.vector(as.matrix(as(obj, "data.frame")[zcol]))
	args.xyplot = fill.call.groups(args.xyplot, z = z, ...)
	scales = longlat.scales(obj, scales, xlim, ylim)
	plt = do.call("xyplot", args.xyplot)
	if (!(is.logical(identify) && identify==FALSE) && interactive()) {
		print(plt)
		if (!(is.numeric(identify) && length(identify) == 2))
			identify = c(1,1)
		trellis.focus("panel", identify[1], identify[2])
		labels = row.names(as(sdf, "data.frame"))
		cat("left-mouse to identify points; right-mouse to end\n")
		cc = coordinates(obj)
		ret = panel.identify(cc[,1], cc[,2], labels)
		trellis.unfocus()
		return(ret)
	} else
		plt
}
setMethod("spplot", signature("SpatialPointsDataFrame"), spplot.points)

panel.gridplot = function(x, y, z, subscripts, ..., panel.number, sp.layout) {
	# set first = TRUE defaults for polygons objects in sp.layout:
	if (!missing(sp.layout) && inherits(sp.layout, "list")) {
		if (inherits(sp.layout[[1]], "list")) {
			for (i in seq(along = sp.layout)) {
				if (inherits(sp.layout[[i]], "list")) {
					sp.i = sp.layout[[i]]
					if (is.null(sp.i$first) && sp.i[[1]] == "sp.polygons")
						sp.layout[[i]]$first = TRUE
				}
			}
		} else if (is.null(sp.layout$first) && sp.layout[[1]] == "sp.polygons")
			sp.layout$first = TRUE
	}
	# print(sp.layout)
	sp.panel.layout(sp.layout, panel.number, first = TRUE)
	panel.levelplot(x, y, z, subscripts, ...)
	sp.panel.layout(sp.layout, panel.number)
}

panel.polygonsplot =
function (x, y, z, subscripts, at = pretty(z), shrink, labels = NULL, 
   		label.style = c("mixed", "flat", "align"), contour = FALSE, 
   		region = TRUE, col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
   		cex = add.text$cex, font = add.text$font, fontfamily = add.text$fontfamily, 
   		fontface = add.text$fontface, col.text = add.text$col, ..., 
   		col.regions = regions$col, alpha.regions = regions$alpha, 
		grid.polygons, sp.layout, panel.number) 
{
	regions <- trellis.par.get("regions")
	add.line <- trellis.par.get("add.line")
	add.text <- trellis.par.get("add.text")
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
			sp.lines2 = function(x, col, ...) lapply(x@Lines, sp.lines3, col, ...)
			for (i in 1:length(grid.polygons@lines))
				sp.lines2(grid.polygons@lines[[i]], col = col.regions[zcol[i]], ...)
		} else {
			pls = getSpPpolygonsSlot(grid.polygons)
   			pO = getSpPplotOrderSlot(grid.polygons)
   			for (i in pO) {
       			Srs <- getPolygonsPolygonsSlot(pls[[i]])
       			pOi <- getPolygonsplotOrderSlot(pls[[i]])
       			for (j in pOi) {
					coords = getPolygonCoordsSlot(Srs[[j]])
					if (getPolygonHoleSlot(Srs[[j]])) {
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
	sp.panel.layout(sp.layout, panel.number)
}

panel.pointsplot = function(x, y, subscripts, col, sp.layout, ..., panel.number) {
	sp.panel.layout(sp.layout, panel.number)
	panel.superpose(x, y, subscripts, col = col, ...)
}

fill.call.groups = function(lst, z, ..., cuts, 
	col.regions = trellis.par.get("regions")$col, legendEntries = "", pch, 
	cex = 1, fill = TRUE, do.log = FALSE, key.space = "bottom") 
{
	dots = list(...)
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
	if (!is.null(dots$key))
		lst$key = dots$key
	else
		lst$key = list(points = list(pch = rep(lst$pch, length = n), 
			col = rep(lst$col, length = n), cex = rep(cex, length = n)), 
			text = list(legendEntries))
	if (is.character(key.space))
		lst$key$space = key.space
	else if (is.list(key.space))
		lst$key = append(lst$key, key.space)
	else
		warning("key.space argument ignored (not list or character)")
	return(lst)
}

SpatialPolygons2Grob = function(obj, fill) {
	if (!is(obj, "SpatialPolygons"))
		stop("object is not of class SpatialPolygons")
	x = numeric(0)
	y = numeric(0)
	id = integer(0)
	pls = getSpPpolygonsSlot(obj)
   	pO <- getSpPplotOrderSlot(obj)
	n = 0
   	for (i in pO) {
   		Srs <- getPolygonsPolygonsSlot(pls[[i]])
   		pOi <- getPolygonsplotOrderSlot(pls[[i]])
   		for (j in pOi) {
			n = n + 1
			cc = getPolygonCoordsSlot(Srs[[j]])
			x = c(x, cc[,1])
			y = c(y, cc[,2])
			id = c(id, rep(n, nrow(cc)))
		}
	}
	polygonGrob(x=x, y=y, id=id, gp = gpar(fill = fill))
}

SpatialPolygonsRescale = function(obj, offset, scale = 1, fill = "black", col = "black", plot.grid = TRUE, ...) {
	if (!is(obj, "SpatialPolygons"))
		stop("object is not of class SpatialPolygons")
	if (length(offset) != 2)
		stop("offset should have length 2")
	if (is.list(offset))
		offset = c(offset[[1]], offset[[2]])
	if (length(scale) == 1)
		scale = rep(scale,2)
	pls = getSpPpolygonsSlot(obj)
   	pO = getSpPplotOrderSlot(obj)
	fill = rep(fill, length = length(pls))
   	for (i in pO) {
   		Srs <- getPolygonsPolygonsSlot(pls[[i]])
   		pOi <- getPolygonsplotOrderSlot(pls[[i]])
   		for (j in pOi) {
			cc = getPolygonCoordsSlot(Srs[[j]])
			x = offset[1] + (cc[,1] * scale[1])
			y = offset[2] + (cc[,2] * scale[2])
			if (plot.grid) {
				grid.polygon(x, y, default.units = "native", 
					gp = gpar(col = col, fill = fill[i], ...))
			} else {
				polygon(x, y, col = fill[i])
				lines(x, y, col = col)
			}
		}
	}
}

mapLegendGrob <- function(obj, widths = unit(1, "cm"), heights = unit(1, "cm"),
		fill = "black", just = "right") {
	grb = SpatialPolygons2Grob(obj, fill)
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
	SpatialPolygons(list(Polygons(list(Polygon(cbind(x1,y1)), Polygon(cbind(rev(x2),rev(y2)))), ID="north")))
}
# north.arrow = .north.arrow()

layout.scale.bar = function(height = 0.05) {
	x1 = c(0, 0.5, 0.5, 0, 0)
	y1 = c(0, 0, height, height, 0)
	x2 = x1 + 0.5
	y2 = y1
	SpatialPolygons(list(Polygons(list(Polygon(cbind(x1,y1))), ID="left"), 
			Polygons(list(Polygon(cbind(rev(x2),rev(y2)))), ID="right")))
}
# scale.bar = .scale.bar()

sp.theme = function(set = FALSE, regions = list(col = bpy.colors(100)), ...) {
	lst = list(regions = regions, ...)
	if (set)
		trellis.par.set(lst)
	else
		lst
}

spplot.key = function(sp.layout, rows = 1, cols = 1) {
	for (i in seq(along=rows)) {
		for (j in seq(along=cols)) {
			trellis.focus("panel", cols[j], rows[i], highlight = FALSE)
			sp.panel.layout(sp.layout)
			trellis.unfocus()
		}
	}
}

#sp.pagefn = function(n) {
#	pos = lattice:::lattice.getStatus("current.panel.positions")
#	spplot.key(sp.layout, pos[1], pos[2])
#}

longlat.scales = function(obj, scales, xlim, ylim) {
	isp = is.projected(obj)
	if (scales$draw && !is.na(isp) && !isp) {
		# lat long -- x:
		if (is.null(scales$x))
			scales$x = list()
		if (is.null(scales$x$at))
			scales$x$at = pretty(xlim)
		if (is.null(scales$x$labels)) {
        	pos = sign(scales$x$at) + 2
        	dir = c("W", "", "E")
        	scales$x$labels = parse(text = paste(abs(scales$x$at), "*degree*", dir[pos]))
		}
		# lat long -- y:
		if (is.null(scales$y))
			scales$y = list()
		if (is.null(scales$y$at))
			scales$y$at = pretty(ylim)
		if (is.null(scales$y$labels)) {
        	pos = sign(scales$y$at) + 2
        	dir = c("S", "", "N")
        	scales$y$labels = parse(text = paste(abs(scales$y$at), "*degree*", dir[pos]))
		}
	}
	scales
}

bbexpand = function(x, fraction) {
	r = diff(x)
	c(x - fraction * r, x + fraction * r)
}

colorkey.factor = function(f, colorkey = list()) {
	f.num = as.numeric(f)
	f.range = range(f.num)
	at = seq(f.range[1]-0.5, f.range[2]+0.501)
	at.labels = seq(f.range[1], f.range[2])
	lf = levels(f)
	colorkey=append(colorkey, list(labels=list(at=at.labels,labels=lf), 
		height=min(1, .05 * length(lf))))
	list(at = at, colorkey = colorkey)
}
