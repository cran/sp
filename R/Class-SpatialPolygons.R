
setClass("Polygon",
	representation("Line", labpt = "numeric", 
	area = "numeric", hole = "logical", ringDir = "integer"),
#	prototype = list(bbox = matrix(rep(NA, 6), 3, 2, 
#			dimnames = list(NULL, c("min","max"))),
#		proj4string = CRS(as.character(NA)),
#		coords = matrix(0),
#		labpt = as.numeric(NA),
#		area = as.numeric(NA),
#		hole = as.logical(NA),
#		ringDir = as.integer(NA)),
	validity = function(object) {
		coords <- coordinates(object)
		start <- coords[1,]
		final <- coords[nrow(coords),]
		if (!identical(start, final)) stop("ring not closed")
		return(TRUE)
})

setClass("Polygons",
	representation(#"Spatial", 
Polygons = "list", plotOrder = "integer", 
	labpt = "numeric", ID = "character", area = "numeric"),
#	prototype = list(bbox = matrix(rep(NA, 6), 3, 2, 
#			dimnames = list(NULL, c("min","max"))),
#		proj4string = CRS(as.character(NA)),
#		Polygons = list(), plotOrder = as.integer(NA),
#		labpt = as.numeric(NA),
#		ID = as.character(NA),
#		area = as.numeric(NA)),
	validity = function(object) {
		if (any(sapply(object@Polygons, function(x) !is(x, "Polygon"))))				stop("not a list of Polygon objects")
		if (length(object@Polygons) != length(object@plotOrder))
			stop("plotOrder and Polygons differ in length")
		return(TRUE)
})

setClass("SpatialPolygons",
representation("Spatial",
		polygons = "list",
		plotOrder = "integer"),
	prototype = list(
		bbox = matrix(rep(NA, 6), 3, 2, dimnames = list(NULL, c("min","max"))),
		proj4string = CRS(as.character(NA)),
		polygons = list(), 
		plotOrder = integer(0)),
	validity = function(object) {
		if (length(object@polygons) != length(object@plotOrder))
			return("length mismatch")
		if (any(unlist(lapply(object@polygons, function(x) 
				!is(x, "Polygons"))))) 
			return("polygons not Polygons objects")
		if (length(object@polygons) != 
			length(unique(getSpPPolygonsIDSlots(object)))) 
				return("non-unique Polygons ID slot values")
		return(TRUE)
	}
)

getPolygonCoordsSlot <- function(Polygon) Polygon@coords

getPolygonLabptSlot <- function(Polygon) Polygon@labpt

getPolygonAreaSlot <- function(Polygon) Polygon@area

getPolygonHoleSlot <- function(Polygon) Polygon@hole

getPolygonsPolygonsSlot <- function(Polygons) Polygons@Polygons

getPolygonsplotOrderSlot <- function(Polygons) Polygons@plotOrder

getPolygonsLabptSlot <- function(Polygons) Polygons@labpt

getPolygonsIDSlot <- function(Polygons) Polygons@ID

getSpPpolygonsSlot <- function(SpP) SpP@polygons

getSpPplotOrderSlot <- function(SpP) SpP@plotOrder

getSpPPolygonsLabptSlots <- function(SpP) {
	Srs <- getSpPpolygonsSlot(SpP)
	t(sapply(Srs, getPolygonsLabptSlot))
}

getSpPPolygonsIDSlots <- function(SpP) {
	Srs <- getSpPpolygonsSlot(SpP)
	sapply(Srs, getPolygonsIDSlot)
}

getSpPnParts <- function(SpP) {
	Srs <- getSpPpolygonsSlot(SpP)
	sapply(Srs, function(x) length(getPolygonsPolygonsSlot(x)))
}


