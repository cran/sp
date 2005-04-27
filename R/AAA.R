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
