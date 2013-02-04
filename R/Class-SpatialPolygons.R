setClass("Polygon",
	representation("Line", labpt = "numeric", 
	area = "numeric", hole = "logical", ringDir = "integer"),
	validity = function(object) {
                res <- .Call(Polygon_validate_c, object)
                res
#		coords <- object@coords
#		start <- coords[1,]
#		final <- coords[nrow(coords),]
#		if (!identical(start, final)) 
#			return("ring not closed")
#		if (any(!is.finite(object@labpt)))
#                    return("infinite label point")
#		return(TRUE)
	}
)

setClass("Polygons",
	representation(Polygons = "list", plotOrder = "integer", 
	labpt = "numeric", ID = "character", area = "numeric"),
	validity = function(object) {
                res <- .Call(Polygons_validate_c, object)
                res
#		if (any(sapply(object@Polygons, function(x) !is(x, "Polygon"))))
#			return("not a list of Polygon objects")
#		if (length(object@Polygons) != length(object@plotOrder))
#			return("plotOrder and Polygons differ in length")
#		if (any(!is.finite(object@labpt)))
#                    return("infinite label point")
#		return(TRUE)
	}
)

setClass("SpatialPolygons",
	representation("Spatial",
		polygons = "list",
		plotOrder = "integer"),
	prototype = list(
		bbox = matrix(rep(NA, 4), 2, 2, dimnames = list(NULL, c("min","max"))),
		proj4string = CRS(as.character(NA)),
		polygons = list(), 
		plotOrder = integer(0)),
	validity = function(object) {
#		if (length(object@polygons) != length(object@plotOrder))
#			return("length mismatch")
#		if (any(unlist(lapply(object@polygons, function(x) 
#				!is(x, "Polygons"))))) 
#			return("polygons not Polygons objects")
#                pls <- slot(object, "polygons")
#                IDs <- sapply(pls, slot, "ID")
                IDs <- .Call(SpatialPolygons_getIDs_c, object)
		if (anyDuplicated(IDs))
			return("non-unique Polygons ID slot values")
                res <- .Call(SpatialPolygons_validate_c, object)
                res
#		if (length(object@polygons) != 
#			length(unique(sapply(slot(object, "polygons"), 
#                            function(i) slot(i, "ID"))))) 
#				return("non-unique Polygons ID slot values")
#		return(TRUE)
	}
)

getPolygonCoordsSlot <- function(Polygon) {
    .Deprecated("slot", msg="use *apply and slot directly")
    Polygon@coords
}

getPolygonLabptSlot <- function(Polygon)  {
    .Deprecated("slot", msg="use *apply and slot directly")
   Polygon@labpt
}

getPolygonAreaSlot <- function(Polygon)  {
    .Deprecated("slot", msg="use *apply and slot directly")
   Polygon@area
}

getPolygonHoleSlot <- function(Polygon)  {
    .Deprecated("slot", msg="use *apply and slot directly")
   Polygon@hole
}

getPolygonsPolygonsSlot <- function(Polygons) {
    .Deprecated("slot", msg="use *apply and slot directly")
   Polygons@Polygons
}

getPolygonsplotOrderSlot <- function(Polygons) {
    .Deprecated("slot", msg="use *apply and slot directly")
   Polygons@plotOrder
}

getPolygonsLabptSlot <- function(Polygons) {
    .Deprecated("slot", msg="use *apply and slot directly")
   Polygons@labpt
}

getPolygonsIDSlot <- function(Polygons) {
    .Deprecated("slot", msg="use *apply and slot directly")
   Polygons@ID
}

getSpPpolygonsSlot <- function(SpP) {
    .Deprecated("slot", msg="use *apply and slot directly")
   SpP@polygons
}

getSpPplotOrderSlot <- function(SpP) {
    .Deprecated("slot", msg="use *apply and slot directly")
   SpP@plotOrder
}

getSpPPolygonsLabptSlots <- function(SpP) {
    .Deprecated("coordinates", msg="use coordinates method")
	Srs <- slot(SpP, "polygons")
	t(sapply(Srs, function(i) slot(i, "labpt")))
}

getSpPPolygonsIDSlots <- function(SpP) {
    .Deprecated("coordinates", msg="use *apply and slot directly")
	Srs <- slot(SpP, "polygons")
	sapply(Srs, function(i) slot(i, "ID"))
}

getSpPnParts <- function(SpP) {
    .Deprecated("coordinates", msg="use *apply and slot directly")
	Srs <- slot(SpP, "polygons")
	sapply(Srs, function(x) length(slot(x, "Polygons")))
}

getSpPnHoles <- function(SpP) {
    .Deprecated("coordinates", msg="use *apply and slot directly")
	Srs <- slot(SpP, "polygons")
	sapply(Srs, function(x) sapply(slot(x, "Polygons"), 
		function(y) slot(y, "hole")))
}

