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
#		tol <- .Machine$double.eps ^ 0.25
#		W <- bb[1,1] < -180 && 
#		    !isTRUE(all.equal((bb[1, 1] - -180), 0, tolerance = tol))
#		E <- bb[1,2] > 360 && 
#		    !isTRUE(all.equal((bb[1, 2] - 360), 0, tolerance = tol))
#		S<- bb[2,1] < -90 && 
#		    !isTRUE(all.equal((bb[2, 1] - -90), 0, tolerance = tol))
#		N <- bb[2,2] > 90 && 
#		    !isTRUE(all.equal((bb[2, 2] - 90), 0, tolerance = tol))
#		if (any(W || E || S || N)) 

		if (!.ll_sanity(bb))
			stop("Geographical CRS given to non-conformant data")
	}
	sd@proj4string = value;
	sd
}

# split out from proj4string<- and Spatial validity to cover numerical fuzz
# RSB 070216
.ll_sanity <- function(bb) {
	tol <- .Machine$double.eps ^ 0.25
	W <- bb[1,1] < -180 && 
	    !isTRUE(all.equal((bb[1, 1] - -180), 0, tolerance = tol))
	E <- bb[1,2] > 360 && 
	    !isTRUE(all.equal((bb[1, 2] - 360), 0, tolerance = tol))
	S<- bb[2,1] < -90 && 
	    !isTRUE(all.equal((bb[2, 1] - -90), 0, tolerance = tol))
	N <- bb[2,2] > 90 && 
	    !isTRUE(all.equal((bb[2, 2] - 90), 0, tolerance = tol))
	return(!(any(W || E || S || N))) 
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
