\name{Lines-class}
\docType{class}
\alias{Lines-class}
\alias{getLinesLinesSlot}
\alias{getLinesIDSlot}
% \alias{coordinates,Lines-method}

\title{Class "Lines" }
\description{ class for sets of line objects }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{Line}
}
\section{Slots}{
  \describe{
    \item{\code{Lines}:}{Object of class \code{"list"}, containing elements
	of class \link{Line-class}}
    \item{\code{ID}:}{Object of class \code{"character"}, unique identifier string}
  }
}
\section{Methods}{
  \describe{
    \item{coordinates}{\code{signature(obj = "Line")}: retrieve coordinates
	from lines; returns list with matrices }
    \item{lines}{\code{signature(x = "Line")}: add lines to a plot}
  }
}
\usage{
getLinesLinesSlot(SL)
getLinesIDSlot(Lines)
}
\arguments{
\item{SL, Lines}{an Lines object}
}


% \references{  }
\author{ Roger Bivand, Edzer Pebesma }
% \note{ }

\seealso{
  \link{Lines-class}, \link{SpatialLines-class}
}
\examples{
}
\keyword{classes}
