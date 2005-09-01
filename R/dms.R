setClass("DMS", representation(WS="logical", deg="numeric", 
	min="numeric", sec="numeric", NS="logical"),
	validity = function(object) {
	    if (object@NS) {
		if (any(abs(object@deg) > 90)) return("abs(degree) > 90")
	    } else {
	        if (any(abs(object@deg) > 360))
	            return("abs(degree) > 360")
	        else if (any(object@WS & (object@deg > 180)))
		    return("degree < -180")
                else return(TRUE)
	    }
	}
)

"dd2dms" <- function(dd, NS=FALSE) {
	sdd <- sign(dd)
	WS <- ifelse(sdd < 0, TRUE, FALSE) 
	dd <- abs(dd)
	deg <- as(floor(dd), "integer")
	dd <- (dd - deg)*60
	min <- as(floor(dd), "integer")
	sec <- (dd - min)*60
	tst <- abs(sec - 60.0) > sqrt(.Machine$double.eps)
        sec <- ifelse(tst, sec, 0.0)
	min <- ifelse(tst, min, min+1)
	tst <- min < 60
        min <- ifelse(tst, min, 0)
	deg <- ifelse(tst, deg, deg+1)

	dms <- new("DMS", WS=WS, deg=deg, min=min, sec=sec, NS=NS)
	tst <- validObject(dms)
	if (is(tst, "logical") & tst) return(dms)
	else stop(tst)
	dms
}

as.double.DMS <- function(x, ...) {
	dd <- x@deg + x@min/60 + x@sec/3600
	dd <- ifelse(x@WS, -dd, dd)
	dd
}

as.numeric.DMS <- function(from) {
	if (!inherits(from, "DMS")) stop("not a DMS object")
	as.double.DMS(from)
}

setAs("DMS", "numeric", as.numeric.DMS)

as.character.DMS <- function(from) {
	if (!inherits(from, "DMS")) stop("not a DMS object")
	if (!from@NS) tag <- c("W", "E")
	else tag <- c("S", "N")
	res <- ifelse(from@WS, tag[1], tag[2])
	res <- paste(ifelse(round(from@sec, digits=3) != "0", 
		paste(round(from@sec, digits=3), '\"', sep=""), ""), 
		res, sep="")
	res <- paste(ifelse(((from@min != 0) | 
		(round(from@sec, digits=3) != "0")),
		paste("d", from@min, "\'", sep=""), ""), res, sep="")
	res <- paste(from@deg, res, sep="")
	invisible(res)
}

setAs("DMS", "character", function(from) {
	if (!from@NS) tag <- c("W", "E")
	else tag <- c("S", "N")
	res <- ifelse(from@WS, tag[1], tag[2])
	res <- paste(ifelse(round(from@sec, digits=3) != "0", 
		paste(round(from@sec, digits=3), '\"', sep=""), ""), 
		res, sep="")
	res <- paste(ifelse(((from@min != 0) | 
		(round(from@sec, digits=3) != "0")),
		paste("d", from@min, "\'", sep=""), ""), res, sep="")
	res <- paste(from@deg, res, sep="")
	invisible(res)
    }
)


setMethod("show", "DMS", function(object) print.DMS(object))

"print.DMS" <- function(x, ...)
{
	res <- as(x, "character")
	print(res, quote=FALSE)
	invisible(res)
}

"char2dms" <- function(from, chd="d", chm="'", chs='"') {
	x <- substr(from, nchar(from), nchar(from))
	NS <- any(x == "N" | x == "S")
	y <- substr(from, 1, nchar(from)-1)
	ndeg <- regexpr(chd, y)
	nmin <- regexpr(chm, y)
	nsec <- regexpr(chs, y)
	deg <- as(substr(y, 1, ndeg-1), "integer")
	min <- as(ifelse(nmin < 1, 0, substr(y, ndeg+1, nmin-1)), "integer")
	sec <- as(ifelse(nsec < 1, 0, substr(y, nmin+1, nsec-1)), "numeric")
	
	WS <- ifelse(x == "W" | x == "S", TRUE, FALSE)
	    
	dms <- new("DMS", WS=WS, deg=deg, min=min, sec=sec, NS=NS)
	tst <- validObject(dms)
	if (is(tst, "logical") & tst) return(dms)
	else stop(tst)
	dms
}
