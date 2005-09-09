#require(methods)
#
#.First.lib <- function(libname, pkgname){
#    library.dynam("sp", pkgname, libname)
#}
#EJP--uncomment, so setMethod("[", etc ) and as(x, "data.frame") will work:
#.noGenerics <- TRUE 

.onLoad <- function(lib, pkg) require(methods)

.onUnload <- function(libpath)
    library.dynam.unload("sp", libpath)

if (!exists("is.R")) {
  is.R = function() {
 	exists("version") && !is.null(vl <- version$language) && vl == "R"
  }
  which.max = function(x) {
  	ix = 1:length(x)
	mx = ix[x == max(x)]
	mx[1]
  }
  NCOL = function(x) {
  	if (is.array(x) && length(dim(x)) > 1 || is.data.frame(x)) ncol(x) else as.integer(1)
  }
}
