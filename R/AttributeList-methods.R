AttributeList = function(x) new("AttributeList", att = x)

as.data.frame.AttributeList = function(x, row.names=NULL, optional=FALSE, ...)
	as.data.frame(x@att, row.names, optional, ...)

setAs("AttributeList", "data.frame", function(from) as.data.frame(from))

setAs("AttributeList", "list", function(from) from@att)

setAs("data.frame", "AttributeList", function(from) AttributeList(as.list(from)))

setAs("list", "AttributeList", function(from) AttributeList(from))

as.list.AttributeList = function(x, ...) x@att

dim.AttributeList = function(x) c(length(x@att[[1]]), length(x@att))

setMethod("[", "AttributeList", function(x, i, j, ... , drop = TRUE) {
	missing.i = missing(i)
	missing.j = missing(j)
	drop <- FALSE
#	if (drop)
#		stop("coerce to data.frame first for drop = TRUE")
	nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
	if (missing.i && missing.j) {
		i = TRUE
		j = TRUE
	} else if (missing.j && !missing.i) { 
		if (nargs == 2) {
			j = i
			i = TRUE
		} else {
			j = TRUE
		}
	} else if (missing.i && !missing.j)
		i = TRUE
	if (is.matrix(i))
		stop("matrix argument not supported in AttributeList selection")
	if (is.character(j) && any(is.na(match(j, names(x)))))
		stop("undefined columns selected")
	if (is.numeric(j) && any(is.na(match(j[j>0], 1:length(x@att)))))
		stop("undefined columns selected")
	AttributeList(lapply(x@att[j], function(x,i) x[i], i))
})

"[[.AttributeList" =  function(x, ...) x@att[[...]]

"[[<-.AttributeList" =  function(x, i, j, value) { 
	if (!missing(j))
		stop("only valid calls are x[[i]] <- value")
	if (!is.null(value)) {
		stopifnot(is.vector(value) || is.factor(value))
		stopifnot(is.na(match(mode(value), c("raw", "list"))))
		stopifnot(length(value) == length(x@att[[1]]))
	}
	x@att[[i]] = value
	x 
}

"$.AttributeList" = function(x,name) x@att[[name]]

"$<-.AttributeList" = function(x, i, value) { 
	if (!is.null(value)) {
		stopifnot(is.vector(value) || is.factor(value))
		stopifnot(is.na(match(mode(value), c("raw", "list"))))
		stopifnot(length(value) == length(x@att[[1]]))
	}
	x@att[[i]] = value
	x 
}

summ.AttributeList = function(object, ...) summary.data.frame(object@att)
setMethod("summary", "AttributeList", summ.AttributeList)

names.AttributeList = function(x) { names(x@att) }

"names<-.AttributeList" = function(x, value) { names(x@att)<-value; x }

if (!is.R()) {
	setMethod("ncol", "AttributeList", function(x)length(x@att))
	setMethod("nrow", "AttributeList", function(x)length(x@att[[1]]))
}
