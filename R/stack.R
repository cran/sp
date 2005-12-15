"spmap.to.lev" <- function (data, zcol = 1:n, n = 2, names.attr)
{
	if (!(is(data, "SpatialPointsDataFrame") || (is(data, "SpatialGridDataFrame"))))
		stop("data is not of a class that extends SpatialPointsDataFrame")

	if (dimensions(data) > 2) {
		warning("spmap.to.lev ignores spatial dimensions beyond the first 2")
		cc = coordinates(data)[,1:2]
		data = as(data, "data.frame")
		coordinates(data) = cc
	}
	coord.names = dimnames(data@coords)[[2]]

	data = stack(as(data, "SpatialPointsDataFrame"), zcol) # replace with data.frame
	if (!missing(names.attr)) {
		if (length(names.attr) != length(zcol))
			stop("length names.attr should match length of zcol")
		data$ind = factor(as.integer(data$ind), labels = names.attr)
	}
	names(data) = c(coord.names, "z", "name")
	data
}

stack.AttributeList = function(x, ...) {
	ns = names(x)
	if (all(unlist(lapply(x@att, is.factor)))) {
		# the expensive but robust way:
		x = stack(lapply(x@att, as.character), ...)
		x$values = factor(x$values)
	} else
		x = stack(lapply(x@att, as.numeric), ...)
	x$ind = factor(x$ind, levels = ns) # don't sort alphabetically
	x
}

stack.SpatialPointsDataFrame = function (x, select, ...)
{
	xd = x@data
   	cc = coordinates(x)
	cc.names = dimnames(cc)[[2]]

	#if (!missing(select)) {
	#	if (!is.numeric(select)) {
	#		nl = as.list(1:ncol(xd))
	#		names(nl) = names(xd)
	#		vars = eval(substitute(select), nl, parent.frame())
	#	} else
	#		vars = select
	#	xd = xd[, vars, drop = FALSE]
	#}
	#xd = xd[, unlist(lapply(xd, is.vector)), drop = FALSE]

	if (!missing(select))
		xd = xd[select]
	nc = ncol(xd)
	xd = stack(xd)

	ccr = data.frame(rep(cc[,1], nc))
	for (i in 2:ncol(cc))
		ccr = data.frame(ccr, rep(cc[,i], nc))
	names(ccr) = cc.names
	#data.frame(ccr, values = unlist(unname(xd)),
	#	ind = factor(rep(names(xd), lapply(xd, length)), 
	#		levels = names(xd)))
	data.frame(ccr, xd)
}

stack.SpatialGridDataFrame = function (x, select, ...)
	stack(as(x, "SpatialPointsDataFrame"), select, ...)

stack.SpatialPixelsDataFrame = function (x, select, ...)
	stack(as(x, "SpatialPointsDataFrame"), select, ...)
