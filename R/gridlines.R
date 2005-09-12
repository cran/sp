gridlines = function(x, easts = pretty(bbox(x)[1,]), 
	norths = pretty(bbox(x)[2,]), ndiscr = 20) 
{
	bb = bbox(x)
	easts <- easts[easts > bb[1,1] & easts < bb[1,2]]
	eastlist <- vector(mode="list", length=length(easts))
	for (i in 1:length(easts))
		eastlist[[i]] <- Line(cbind(rep(easts[i], ndiscr), 
			seq(bb[2,1], bb[2,2], length.out=ndiscr)))
	norths <- norths[norths > bb[2,1] & norths < bb[2,2]]
	northlist <- vector(mode="list", length=length(norths))
	for (i in 1:length(norths)) 
		northlist[[i]] <- Line(cbind(seq(bb[1,1], bb[1,2], length.out=ndiscr), 
			rep(norths[i], ndiscr)))
	SpatialLines(list(Lines(northlist, "NS"), Lines(eastlist, "EW")), 
		CRS(proj4string(x)))
}
