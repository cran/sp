# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("merge")) {
    setGeneric("merge", function(x, y, ...)
        standardGeneric("merge"))
}

setMethod('merge', signature(x='Spatial', y='data.frame'), 
  function(x, y, by=intersect(names(x), names(y)), by.x=by, 
		by.y=by, all.x=TRUE, suffixes = c(".x",".y"), 
		incomparables = NULL, ...) {
	if (!('data' %in% slotNames(x)))
		stop('x has no attributes')
	d <- x@data
	d$donotusethisvariablename976 <- 1:nrow(d)
	
	y <- unique(y)
	i <- apply(y[, by.y, drop=FALSE], 1, paste) %in% 
			apply(x@data[, by.x, drop=FALSE], 1, paste)
	y <- y[i, ,drop=FALSE]
	if (isTRUE(any(table(y[, by.y]) > 1)))
		stop("'y' has multiple records for one or more 'by.y' key(s)")
	
	if (!all.x)
		y$donotusethisvariablename679 <- 1
	
	d <- merge(d, y, by=by, by.x=by.x, by.y=by.y, suffixes=suffixes, 
		incomparables=incomparables, all.x=TRUE, all.y=FALSE)
	d <- d[order(d$donotusethisvariablename976), ]
	d$donotusethisvariablename976 <- NULL
	rownames(d) <- row.names(x)
	x@data <- d

	if (! all.x) {
		x <- x[!is.na(x@data$donotusethisvariablename679), ,drop=FALSE] 
		x@data$donotusethisvariablename679 <- NULL
	}
	x
} 
)
