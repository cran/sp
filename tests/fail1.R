library(sp)
data(meuse)
x = meuse

coordinates(x) <- c("x", "y")
try(proj4string(x) <- 1.5)
try(coordinates(a) <- cbind(1:10, 10:1))
# fails because a is not found; passes if a assigned NULL, see pass1.R

x = meuse
# invalid coordinate formulae:
try(coordinates(x) <- ~log(x)+sqrt(y)) # no expressions allowed
try(coordinates(x) <- ~x+y+z) # z is not present
x$x2 = x$x^2
x$y2 = x$y^2
try(coordinates(x) <- ~x+y+x2+y2) # 4D now passes check...
x = meuse
try(coordinates(x) <- ~x) # 1D not allowed

# is.na.sp.coords
a = data.frame(cbind(xx=c(1,NA,2,10),yy=c(2,NA,NA,20)))
try(coordinates(a) <- c("xx", "yy")) # should fail!

x = meuse[1:4,]
coordinates(x) = c(1,2)
# this should fail -- zinc is not a row:
#(and will break automatic testing, so outcommented!)
#try(q <- x["zinc",])
# this will issue warning under S-Plus, or a silent rename under R
try(x[c("zinc", "copper", "zinc")])

# this will fail, as "x" is not in the data part:
try(x[c("zinc", "x", "copper", "zinc")])

xx = data.frame(x=1:10, y=1:10)

# fails; use SpatialPoints() to create points without attribute 
try(coordinates(xx) <- c("x", "y")) 

x = matrix(3, 5, 2)
dimnames(x) = list(c(1,1:4), NULL)
y = data.frame(a = 1:5, b = 5:1)
try(SpatialPointsDataFrame(x, y)) # will complain:
SpatialPointsDataFrame(x, y, match.ID = FALSE) # won't complain

Sr1 = Sring(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Sring(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Sring(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Sring(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Srings(list(Sr1), "s1")
Srs2 = Srings(list(Sr2), "s2")
Srs3 = Srings(list(Sr3, Sr4), "s2")
try(SR <- SpatialRings(list(Srs1,Srs2,Srs3))) # will complain
Srs3 = Srings(list(Sr3, Sr4), "s3/4")
SR = SpatialRings(list(Srs1,Srs2,Srs3)) # won't complain

attr = data.frame(a=1:3, b=3:1, row.names=c("s1", "s2", "s3"))
try(SrDf <- SpatialRingsDataFrame(SR, attr)) # will complain
SrDf = SpatialRingsDataFrame(SR, attr, match.ID = FALSE) # won't complain
attr = data.frame(a=1:3, b=3:1, row.names=c("s1", "s2", "s3/4"))
SrDf = SpatialRingsDataFrame(SR, attr) # won't complain

