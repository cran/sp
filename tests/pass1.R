library(sp)
data(meuse)
x = meuse
coordinates(x) = cbind(rnorm(155), rnorm(155))
# should pass:
names(x@data)
names(as.data.frame(x))
class(as(x, "data.frame"))
x = meuse
# coordinates defined as data:
coordinates(x) = cbind(xcoord = rnorm(155), ycoord = rnorm(155))
# should pass:
names(x@data)
names(as.data.frame(x))
is.projected(x)
proj4string(x)

set.seed(13131) # make sample reproducable:
x = meuse[, sample(ncol(meuse))] # 'randomly' shuffle columns
# coordinates defined as variable names:
coordinates(x) = c("x", "y") # no matter their position
#plot(x, cex=.05 * sqrt(x@data[,"zinc"]),
plot(x, cex=.05 * sqrt(as.data.frame(x)["zinc"]),
 	xlab="x-coordinate (RDM)", ylab="y-coordinate (RDM)", 
	main = "Meuse: zinc bubble plot")
print(summary(x))

# coordinates defined as formula:
x = meuse[, 1:5]
coordinates(x) = ~x+y
print(summary(x))

# a = NULL
# cc = cbind(sample(1:10), sample(1:10), sample(1:10))
# coordinates(a) = cc
# summary(a)
