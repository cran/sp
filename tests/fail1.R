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

