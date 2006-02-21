proj4string = function(sd) {
	if (!extends(class(sd), "Spatial"))
		stop("proj4string only works for class(es extending) Spatial")
	as.character(sd@proj4string@projargs)
}

"proj4string<-" = function(sd, value) {
	if (!extends(class(sd), "Spatial"))
		stop("proj4string only works for class(es extending) Spatial")
	if (!is(value, "CRS"))
		stop("assigned value must be CRS object")
	p4str <- value@projargs
	ll <- FALSE
	if (!is.na(p4str)) {
		res <- grep("longlat", p4str, fixed=TRUE)
		if (length(res) != 0) ll <- TRUE
	}
	if (ll) {
		bb <- bbox(sd)
		if (any(bb[1,1] < -180 || bb[1,2] > 360 || 
			bb[2,1] < -90 || bb[2,2] > 90)) 
			stop("Geographical CRS given to non-conformant data")
	}
	sd@proj4string = value;
	sd
}

is.projected = function(sd) {
	if (!extends(class(sd), "Spatial"))
		stop("is.projected only works for class(es extending) Spatial")
	p4str <- proj4string(sd)
#ifdef R
	if (is.na(p4str) || nchar(p4str) == 0) 
#else
#S	if (p4str == "NA")  # bloody S-Plus!
#endif
		return(as.logical(NA))
	else {
#ifdef R
		res <- grep("longlat", p4str, fixed=TRUE)
#else
#S		res <- grep("longlat", p4str)
#endif
		if (length(res) == 0)
			return(TRUE)
		else
			return(FALSE)
	}
}
