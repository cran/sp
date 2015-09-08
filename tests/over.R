library(sp)
x = c(0.5, 0.5, 1.2, 1.5)
y = c(1.5, 0.5, 0.5, 0.5)
xy = cbind(x,y)
dimnames(xy)[[1]] = c("a", "b", "c", "d")
pts = SpatialPoints(xy)

xpol = c(0,1,1,0,0)
ypol = c(0,0,1,1,0)
pol = SpatialPolygons(list(
 	Polygons(list(Polygon(cbind(xpol-1.05,ypol))), ID="x1"),
 	Polygons(list(Polygon(cbind(xpol,ypol))), ID="x2"),
 	Polygons(list(Polygon(cbind(xpol,ypol-1.05))), ID="x3"),
 	Polygons(list(Polygon(cbind(xpol+1.05,ypol))), ID="x4"),
 	Polygons(list(Polygon(cbind(xpol+.4, ypol+.1))), ID="x5")
))

zdf = data.frame(z1 = 1:4, z2=4:1, f = c("a", "a", "b", "b"),
 	row.names = c("a", "b", "c", "d"))
zdf
ptsdf = SpatialPointsDataFrame(pts, zdf)

zpl = data.frame(z = c(10, 15, 25, 3, 0), zz=1:5, 
 	f = c("z", "q", "r", "z", "q"), row.names = c("x1", "x2", "x3", "x4", "x5"))
zpl
poldf = SpatialPolygonsDataFrame(pol, zpl)

gt = GridTopology(c(.5,.5), c(1,1), c(3,2))
sg = SpatialGrid(gt)
df6 = data.frame(z = 6:1, f = c("a", "a", "b", "b", "c", "c"))
sgdf = SpatialGridDataFrame(gt, df6)
over(sg, pol)
over(sg, poldf)
over(sg, poldf[1:2])

spix = as(sg, "SpatialPixels")
spixdf = as(sgdf, "SpatialPixelsDataFrame")
over(spix, pol)
over(spix, poldf)
over(spix, poldf[1:2])

over(pol, sg)
over(pol, sgdf)
over(pol, sgdf[1], fn = mean)

over(pol, spix)
over(pol, spixdf)
over(pol, spixdf[1], fn = mean)

over(pts, sg)
over(pts, spix)
over(pts, sgdf)
over(pts, spixdf)

over(sg, sg)
over(sg, spix)
over(sg, sgdf)
over(sg, spixdf)

over(spix, sg)
over(spix, spix)
over(spix, sgdf)
over(spix, spixdf)
