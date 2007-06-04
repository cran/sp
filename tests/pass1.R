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
plot(x, cex=.05 * sqrt(as.data.frame(x)[["zinc"]]),pch=1)
title("Meuse: zinc bubble plot")
print(summary(x))

# coordinates defined as formula:
x = meuse[, 1:5]
coordinates(x) = ~x+y
print(summary(x))

# a = NULL
# cc = cbind(sample(1:10), sample(1:10), sample(1:10))
# coordinates(a) = cc
# summary(a)

xx = SpatialPointsDataFrame(matrix(1:10,5,2),data.frame(f = 1:5))
rbind(xx,xx,xx,xx)

grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as.SpatialPolygons.GridTopology(grd)
summary(rbind(polys[1:10], polys[11:20], polys[21:30]))
plot(rbind(polys[1:10],polys[21:30]))
title("2 x 10 blocks -- test rbind on SpatialPolygons")

l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
Sl = SpatialLines(list(S1,S2))

summary(as(polys, "SpatialLines"))
summary(as(Sl, "SpatialPoints"))
summary(as(Sl, "SpatialPointsDataFrame"))
SlDf = SpatialLinesDataFrame(Sl, data.frame(xx = c("foo", "bar")), match.ID = FALSE)
summary(as(SlDf, "SpatialPointsDataFrame"))

meuse[["xxx"]] = log(meuse$zinc)
summary(meuse)
meuse$xxy = log(meuse[["zinc"]])
summary(meuse)
