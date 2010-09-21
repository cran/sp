setMethod("proj4string", signature(obj = "Spatial"),
	function(obj) 
		as.character(obj@proj4string@projargs)
)

setReplaceMethod("proj4string", signature(obj = "Spatial", value = "CRS"),
	function(obj, value) {
		p4str <- value@projargs
		ll <- FALSE
		if (!is.na(p4str)) {
			res <- grep("longlat", p4str, fixed=TRUE)
			if (length(res) != 0) ll <- TRUE
		}
		if (ll) {
			bb <- bbox(obj)
			if (!.ll_sanity(bb))
				stop("Geographical CRS given to non-conformant data")
		}
		obj@proj4string = value;
		obj
	}
)

setReplaceMethod("proj4string", signature(obj = "Spatial", value = "character"),
	function(obj, value) {
		value = CRS(value)
		proj4string(obj) = value
		obj
	}
)


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

setMethod("is.projected", signature(obj = "Spatial"),
	function(obj) {
		p4str <- proj4string(obj)
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
)

#is.projected = function(obj) {
#	if (!extends(class(obj), "Spatial"))
#		stop("is.projected only works for class(es extending) Spatial")
