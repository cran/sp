# Copyright (c) 2003-6 by Barry Rowlingson and Roger Bivand

if (!is.R()) {
  strsplit <- function(a,b) {
    if (a == as.character(NA))
        return(as.character(NA))
    else list(unlist(unpaste(a, b)))
  }
}

"CRS" <- function(projargs) {
    if (nchar(projargs) == 0) projargs <- as.character(NA)
    if (is.na(projargs)) uprojargs <- projargs
    else uprojargs <- paste(unique(unlist(strsplit(projargs, " "))), 
	collapse=" ")
    if (length(grep("= ", uprojargs)) != 0)
	stop(paste("No spaces permitted in PROJ4 argument-value pairs:", 
	    uprojargs))
    if (length(grep(" [:alnum:]", uprojargs)) != 0)
	stop(paste("PROJ4 argument-value pairs must begin with +:", 
	    uprojargs))
    if (length(grep("rgdal", search()) > 0) &&
      (sessionInfo()$otherPkgs$rgdal$Version > "0.4-2")) {
	if (!is.na(uprojargs)) {
		res <- .Call("checkCRSArgs", uprojargs, 
				PACKAGE="rgdal")
	} else res <- list(TRUE, as.character(NA))
	if (!res[[1]]) stop(res[[2]])
	else uprojargs <- res[[2]]
    }
    res <- new("CRS", projargs=uprojargs)
    res
}

"print.CRS" <- function(x, ...)
{
	cat("CRS arguments:", x@projargs, "\n")
}

setMethod("show", "CRS", function(object) print.CRS(object))
