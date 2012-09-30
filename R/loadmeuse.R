loadMeuse = function(gridded = TRUE, river = FALSE) {
   crs = CRS("+init=epsg:28992")
   meuse = NULL
   meuse.grid = NULL
   meuse.riv = NULL
   data(meuse)
   coordinates(meuse) <<- ~x+y
   data(meuse.grid)
   if (gridded) {
     gridded(meuse.grid) <<- ~x+y
  } else 
	 coordinates(meuse.grid) <<- ~x+y
  if (river) {
	rm(meuse.riv)
    data(meuse.riv)
    meuse.riv <<- SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)),"meuse.riv")))
    proj4string(meuse.riv) <<- crs
  }
  proj4string(meuse) <<- crs
  proj4string(meuse.grid) <<- crs
  invisible(NULL)
}
