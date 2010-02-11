loadMeuse = function(gridded = TRUE) {
   meuse = NULL
   meuse.grid = NULL
   data(meuse)
   coordinates(meuse) <<- ~x+y
   data(meuse.grid)
   if (gridded) {
     gridded(meuse.grid) <<- ~x+y
  } else 
	 coordinates(meuse.grid) <<- ~x+y
  invisible(NULL)
}
