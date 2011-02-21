mapLegendGrob2 <- function(obj, widths = unit(1, "cm"), heights = unit(1, "cm"),
        fill = "black", just = "right", labels) {
    grb1 = SpatialPointsLegend(obj, fill = fill)
	grb2 = TextLegend(labels)
    key.layout <- grid.layout(nrow = 1, ncol = 1, widths = widths,
                    heights = heights, respect = TRUE, just = just)
    key.gf <- frameGrob(layout = key.layout)
#   key.gf <- placeGrob(key.gf,
#                 rectGrob(gp = gpar(fill = "transparent", col = NULL)),
#                 row = NULL, col = NULL)
    key.gf <- placeGrob(key.gf, grb1, row = 1, col = 1)
    key.gf <- placeGrob(key.gf, grb2, row = 1, col = 1)
    key.gf
}

TextLegend = function(labels, dist = 0.5) {
	x = rep(dist, length(labels))
	y = 1:length(labels)
	textGrob(labels, x, y, just = "left")
}
SpatialPointsLegend = function(pch, fill) {
	x = rep(0, length(pch))
	y = 1:length(pch)
	pointsGrob(x=x, y=y, pch=pch, gp = gpar(fill = fill))
}
spplot(spdfMeuse,
	sp.layout = list("sp.points", meuse, pch = as.integer(meuse $soil)),
	legend = list(inside = list(fun = mapLegendGrob2(1:3, fill=1:3, 
	labels=c("a","b","c")), x = 1, y = 0.1, corner = c(0,0))))
