setClass("AttributeList",
	representation(att = "list"),
	prototype = list(NULL),
	validity = function(object) {
		ln = sapply(object@att, length)
		if (length(unique(ln)) > 1)
			stop("AttributeList items not of equal length")
		return(TRUE)
	}
)
