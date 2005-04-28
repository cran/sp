# Copyright (c) 2003-4 by Barry Rowlingson and Roger Bivand

setClass("CRS", representation(projargs = "character"),
    		prototype = list(projargs = character(1)))


"CRS" <- function(projargs) {
    if (is.na(projargs)) uprojargs <- projargs
    else uprojargs <- paste(unique(unlist(strsplit(projargs, " "))), 
	collapse=" ")
    res <- new("CRS", projargs=uprojargs)
    res
}



"print.CRS" <- function(x, ...)
{
	cat("CRS arguments:", x@projargs, "\n")
}

setMethod("show", "CRS", function(object) print.CRS(object))

