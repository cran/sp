\name{AttributeList-class}
\docType{class}
\alias{AttributeList-class}
\alias{AttributeList}
\alias{[,AttributeList-method}
\alias{coerce,AttributeList,data.frame-method}
\alias{summary,AttributeList-method}
\alias{coerce,data.frame,AttributeList-method}
\alias{coerce,list,AttributeList-method}
\alias{coerce,AttributeList,list-method}
\alias{as.list.AttributeList}
\alias{as.data.frame.AttributeList}
\alias{dim.AttributeList}
\alias{[[.AttributeList}
\alias{[[<-.AttributeList}
\alias{$.AttributeList}
\alias{$<-.AttributeList}
\alias{names.AttributeList}
\alias{names<-.AttributeList}

\title{Class "AttributeList" }
\description{ Attribute list; a kind of data.frame but without row.names }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{AttributeList(x)},
with \code{x} a list having elements of equal length.
}
\section{Slots}{
  \describe{
    \item{\code{att}:}{Object of class \code{"list"}, containing the
	attributes variables. The variables are guaranteed to have equal
	length, and can be of any type that has a length }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "AttributeList")}: subsets }
    \item{coerce}{\code{signature(from = "AttributeList", to = "data.frame")}: convert the list
	slot to data.frame }
    \item{coerce}{\code{signature(from = "AttributeList", to = "list")}: retrieve the list slot }
    \item{coerce}{\code{signature(from = "data.frame", to = "AttributeList")}: ... }
    \item{coerce}{\code{signature(from = "list", to = "AttributeList")}: ... }
	\item{as.list}{ coerce AttributeList object to list}
	\item{as.data.frame}{ coerce AttributeList object to data.frame}
	\item{dim}{ retrieve dimensions of object, as if it were a data.frame:
		\code{dim(x)[1]} is the length of variables (nr or rows),
		\code{dim(x)[2]} the number of variables (nr of columns) }
	\item{[[, \$}{retrieve a list element}
	\item{[[<-, \$<-}{assign a list element}
	\item{summary}{\code{signature(object = "AttributeList")} summarize object; 
	calls \code{summary.data.frame} on the list}
	\item{names}{get names from the list}
	\item{names<-}{assign names to the list}
  }
}
\author{ Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl}}

\note{ In earlier versions (sp <= 0.7-6), objects of class
\link{SpatialPointsDataFrame-class}, \link{SpatialPixelsDataFrame-class}
and \link{SpatialGridDataFrame-class} had a slot of class
\link{data.frame} to store the attribute information. It turns out that
especially for larger grids, holding the grid attribute information in a
data.frame requires up to a factor 11 more memory than needed by the data
alone, for the sole reason that R creates unique row.names for each row
(i.e., value).

To get around this problem, we created the simple S4 AttributeList class
which has a single slot which is a list, without row.names, but with a
validator function that checks that all elements in the list have equal
length. The class is meant to behave similar to a data.frame, having a
similar set of methods. It does not automatically convert character data
to factors.

If you have row.names in your data.frame, and you want to keep them when
you convert them to e.g. a \link{SpatialPointsDataFrame-class} object
using \link{coordinates}, you should first copy the row.names to a column
in the data.frame. If not, they will get lost.
}

\seealso{\link{list}, \link{data.frame}}
\examples{
x = AttributeList(list(a = 1:10, b = sample(1:10), c = runif(10)))
x
summary(x)
x = object.size(AttributeList(list(a = 1:1000000)))
x
y = object.size(data.frame(a = 1:1000000))
y
}
\keyword{classes}
