library(sp)
xy = expand.grid(x = 1:4, y = 1:4)
xy.sp = SpatialPoints(xy)
gridded(xy.sp) = T
# deselect 1 row and 2 col:
tst = xy.sp[-c(2,6,10,14,9,11,12)]
tst
# promote to SpatialPixelsDataFrame:
tst$xx = rnorm(9)
spplot(tst["xx"], main = "empty row + col")
