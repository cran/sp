# library(sp)
# library(maptools)
# data(ncshp)
# nc1 <- as.SpatialPolygons.Shapes(nc.shp$Shapes, as.character(nc.shp$att.data$FIPS))
# df <- nc.shp$att.data
# rownames(df) <- as.character(nc.shp$att.data$FIPS)
# identical(rownames(df), getSpPPolygonsIDSlots(nc1))
# nc <- SpatialPolygonsDataFrame(nc1, df)
# 
# nc$X = factor(sample(1:5,100,replace=T),labels=letters[1:5])
# nc$Y = factor(sample(1:5,100,replace=T),labels=letters[6:10])
# spplot(nc, c("X","Y"))
