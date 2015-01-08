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
		incomparables = NULL, duplicateGeoms=FALSE, ...) {

	if (!('data' %in% slotNames(x)))
		stop('x has no attributes')

# email, RJH, 12/24/13, replace:
#	i <- apply(y[, by.y, drop=FALSE], 1, paste) %in% 
#			apply(x@data[, by.x, drop=FALSE], 1, paste)
# by the following block:

## Spatial* objects cannot have NULL geometries
## so we first get the records in y that match to a record in x. 
	i <- apply(y[, by.y, drop=FALSE], 1, 
		function(x) paste(x, collapse='_')) %in% 
		apply(x@data[, by.x, drop=FALSE], 1, 
			function(x) paste(x, collapse='_'))

	if (all(!i)) {
		warning("none of the records in y can be matched to x")
		return(x)
	} else if (sum(!i) > 0) {
		warning(paste(sum(!i), "records in y cannot be matched to x"))
	}
	y <- y[i, ,drop=FALSE]
	
## check for duplicates in by.y	
	if (isTRUE(any(table(y[, by.y]) > 1))) {
		if (!duplicateGeoms) { 
			dy <- nrow(y)
			y <- unique(y)
			if (isTRUE(any(table(y[, by.y]) > 1))) {
				stop("'y' has multiple records for one or more 'by.y' key(s)")
			} else {
				warning(paste(dy - nrow(y), 'duplicate records in y were removed'))
			}
		}
	}
	
	
	x$DoNotUse_temp_sequential_ID_963 <- 1:nrow(x)
	d <- merge(x@data, y, by=by, by.x=by.x, by.y=by.y, suffixes=suffixes, 
		incomparables=incomparables, all.x=all.x, all.y=FALSE)
	d <- d[order(d$DoNotUse_temp_sequential_ID_963), ]
	x <- x[d$DoNotUse_temp_sequential_ID_963, ]
	d$DoNotUse_temp_sequential_ID_963 <- NULL
	x@data <- d
	x
} 
)
