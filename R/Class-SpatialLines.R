setClass("Line", 
	representation(coords = "matrix"),
	prototype = list(coords = matrix(0)),
	validity = function(object) {
		if (any(is.na(object@coords)))
			stop("coords cannot contain missing values")
		if (ncol(object@coords) != 2)
			return("coords should have 2 columns")
#		if (nrow(object@coords) < 2)
#			return("Line should have at least 2 points")
		return(TRUE)
	}
)

setClass("Lines",
	representation(Lines = "list", ID = "character"),
	validity = function(object) {
		if (any(sapply(object@Lines, function(x) !is(x, "Line"))))
			stop("not a list of Line objects")
		return(TRUE)
})

setClass("SpatialLines",
	representation("Spatial", lines = "list"),
	prototype = list(bbox = matrix(rep(NA, 2), 2, 2, 
			dimnames = list(NULL, c("min","max"))),
		proj4string = CRS(as.character(NA)),
		lines = list()),
	validity = function(object) {
		if (any(unlist(lapply(object@lines, function(x) 
			!is(x, "Lines"))))) stop("lines not Lines objects")
		if (any(duplicated(sapply(slot(object, "lines"), function(i) slot(i, "ID")))))
			return("non-unique Lines ID slot values")
#		if (length(object@lines) != 
#			length(unique(sapply(slot(object, "lines"),
#                            function(x) slot(x, "ID"))))) 
#				return("non-unique Lines ID slot values")
		return(TRUE)
	}
)

getSLlinesSlot <- function(SL) {
    .Deprecated("slot", msg="use *apply and slot directly")
    SL@lines
}

getLinesLinesSlot <- function(SL) {
    .Deprecated("slot", msg="use *apply and slot directly")
    SL@Lines
}

getLinesIDSlot <- function(Lines) {
    .Deprecated("slot", msg="use *apply and slot directly")
    Lines@ID
}

getSLLinesIDSlots <- function(SL) {
        .Deprecated("slot", msg="use *apply and slot directly")
	Sls <- slot(SL, "lines")
	sapply(Sls, function(x) slot(x, "ID"))
}


