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
	sd@proj4string = value;
	sd
}

is.projected = function(sd) {
	if (!extends(class(sd), "Spatial"))
		stop("is.projected only works for class(es extending) Spatial")
	p4str <- proj4string(sd)
#ifdef R
	if (is.na(p4str)) 
#else
#%	if (p4str == "NA")  # bloody S-Plus!
#endif
		return(as.logical(NA))
	else {
#ifdef R
		res <- grep("latlong", p4str, fixed=TRUE)
#else
#%		res <- grep("latlong", p4str)
#endif
		if (length(res) == 0)
			return(TRUE)
		else
			return(FALSE)
	}
}
