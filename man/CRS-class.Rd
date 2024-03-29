\name{CRS-class}
\docType{class}
\alias{CRS-class}
\alias{CRS}
\alias{identicalCRS}
\alias{print.CRS}
\alias{show,CRS-method}
\alias{wkt,CRS-method}
\alias{rebuild_CRS}
\alias{rebuild_CRS,CRS-method}
\alias{is.projected,CRS-method}

\title{Class "CRS" of coordinate reference system arguments}

\description{Interface class to the PROJ projection and transformation system. The class is defined as an empty stub accepting value NA in the sp package. The initiation function may call the PROJ library through \pkg{sf} to verify the argument set against those known in the library, returning error messages where necessary. If the \code{"CRS"} object is instantiated using \code{CRS()} with \pkg{sf} using PROJ >= 6 and GDAL >= 3, the object may also have a WKT2 (2019) string carried as a \code{comment}. The arguments for a Proj.4 string must be entered exactly as in the Proj.4 documentation, in particular there cannot be any white space in \code{+<key>=<value>} strings, and successive such strings can only be separated by blanks. Note that only \dQuote{+proj=longlat +ellps=WGS84} is accepted for geographical coordinates, which must be ordered (eastings, northings); the \dQuote{+ellps=} definition must be given (or expanded internally from a given \dQuote{+datum=} value) for recent versions of the Proj.4 library, and should be set to an appropriate value.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{CRS("projargs")}, where "projargs" is a valid string of PROJ.4 arguments. If the argument is a zero-length string or a character \code{NA}, the object records \code{NA}. If the \code{"CRS"} object is instantiated using \code{CRS()} with \pkg{sf} using PROJ >= 6 and GDAL >= 3, the object may also have a WKT2 (2019) string carried as a \code{comment}. The initiation function may call the PROJ library through \pkg{sf} to verify the argument set against those known in the library, returning error messages where necessary.
}
\section{Slots}{
  \describe{
    \item{\code{projargs}:}{Object of class \code{"character"}: projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. }
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "CRS")}: print deprecated Proj.4 projection arguments and WKT2 2019 representation if available }
    \item{wkt}{\code{signature(object = "CRS")}: return WKT comment on object}
    \item{rebuild_CRS}{rebuild a CRS object, usually used to add a WKT comment with PROJ >= 6 and GDAL >= 3}
  }
}
\usage{
CRS(projargs, doCheckCRSArgs=TRUE, SRS_string=NULL, get_source_if_boundcrs=TRUE,
 use_cache=TRUE)
identicalCRS(x,y)
}
\arguments{
\item{projargs}{A character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation; if the projection is unknown, use \code{as.character(NA)}, it may be missing or an empty string of zero length and will then set to the missing value. With \pkg{sf} built with PROJ >= 6 and GDAL >= 3, the \code{+init=} key may only be used with value \code{epsg:<code>}. From \pkg{sp} version 1.4-4, the string associated with the SRS_string argument may be entered as-is and will be set as SRS_string if the projargs argument does not begin with a \code{+} (suggested by Mikko Vihtakari).}
\item{doCheckCRSArgs}{default TRUE}
\item{SRS_string}{default NULL, only used when \pkg{sf} is built with PROJ >= 6 and GDAL >= 3; a valid WKT string or SRS definition such as \code{"EPSG:4326"} or \code{"ESRI:102761"}}
\item{get_source_if_boundcrs}{(from \pkg{rgdal} 1.5-17, default TRUE) The presence of the \code{+towgs84=} key in a Proj4 string \code{projargs=} argument value may promote the output WKT2 CRS to BOUNDCRS for PROJ >= 6 and GDAL >= 3, which is a coordinate operation from the input datum to WGS84. This is often unfortunate, so a PROJ function is called through \pkg{rgdal} to retrieve the underlying source definition.}
\item{use_cache}{default TRUE, if FALSE ignore cached lookup values}
\item{x}{object having a \link{proj4string} method,
or if \code{y} is missing, list with objects that have a \code{proj4string} method}
\item{y}{object of class \link{Spatial}, or having a \link{proj4string} method}
}
\value{
\code{CRS} returns on success an object of class
\link{CRS}. \code{identicalCRS} returns a logical, indicating
whether \code{x} and \code{y} have identical CRS, or if \code{y}
is missing whether all objects in list \code{x}
have identical CRS.
}
\references{\url{https://github.com/OSGeo/PROJ}}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{
Lists of projections may be seen by using the programs installed with the PROJ.4 library, in particular proj and cs2cs; with the latter, -lp lists projections, -le ellipsoids, -lu units, and -ld datum(s) known to the installed software (available in \pkg{sf} using \code{sf_proj_info}). These are added to in successive releases, so tracking the website or compiling and installing the most recent revisions will give the greatest choice. Finding the very important datum transformation parameters to be given with the +towgs84 tag is a further challenge, and is essential when the datums used in data to be used together differ. Tracing projection arguments is easier now than before the mass ownership of GPS receivers raised the issue of matching coordinates from different argument sets (GPS output and paper map, for example). See \code{GridsDatums}, \code{\link[sf]{sf_proj_info}} for help in finding CRS definitions.

The 4.9.1 release of the PROJ library omitted a small file of defaults, leading to reports of \dQuote{major axis or radius = 0 or not given} errors. From 0.9-3, rgdal checks for the presence of this file (proj_def.dat), and if not found, and under similar conditions to those used by PROJ.4, adds \dQuote{+ellps=WGS84} to the input string being checked by \code{checkCRSArgs} The \dQuote{+no_defs} tag ignores the file of defaults, and the default work-around implemented to get around this problem; strings including \dQuote{init} and \dQuote{datum} tags also trigger the avoidance of the work-around. Now messages are issued when a candidate CRS is checked; they may be suppressed using \code{suppressMessages}.

From release 6 of the PROJ library, when used in building \pkg{sf} with GDAL >= 3, the \code{+datum=} key in the Proj.4 string CRS representation is deprecated, and the \code{+towgs84=} and \code{+nadgrids=} keys may be deprecated soon. For this reason, \pkg{sp} and \pkg{sf} are starting to use WKT2 (2019) string representations. In \pkg{sp}, the \code{"CRS"} object in itself remains unchanged, but the content of its \code{"projargs"} slot may be degraded. To work around the degradation, a \code{comment} is added around the \code{"CRS"} object containing a WKT2 (2019) string when \pkg{sf} is available and built with PROJ >= 6 and GDAL >=3.

}


\examples{
CRS()
CRS("")
CRS(as.character(NA))
CRS("+proj=longlat +datum=WGS84")
run <- FALSE
run <- require(sf) 
if (run) {
  print((CRS("+proj=longlat +datum=NAD27")))
}
if (run) {
  print((CRS("+init=epsg:4267")))
}
if (run) {
  print((CRS("+init=epsg:26978")))
}
if (run) {
  print((CRS(paste("+proj=sterea +lat_0=52.15616055555555",
 "+lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel",
 " +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812 +units=m"))))
}
if (run) {
  print((CRS("+init=epsg:28992")))
}
if (run) {
  print((CRS("EPSG:28992")))
}
if (run) {
  print((CRS(SRS_string="EPSG:28992")))
}
if (run) {
  o <- try(CRS(SRS_string="ESRI:102760"))
  if (!inherits(o, "try-error")) print((o))
}
if (run) {
  o <- try(CRS("EPSG:4326"))
  if (!inherits(o, "try-error")) print((o))
}
if (run) {
  o <- try(CRS("ESRI:102760"))
  if (!inherits(o, "try-error")) print((o))
}
if (run) {
o <- new("Spatial")
proj4string(o) <- CRS("+init=epsg:27700")
}
if (run && !is.null(comment(slot(o, "proj4string")))) {
  cat(wkt(o), sep="\n")
  cat(wkt(slot(o, "proj4string")), sep="\n")
}
}
\keyword{classes}
\keyword{spatial}
