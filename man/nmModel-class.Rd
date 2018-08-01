\name{nmModel-class}
\docType{class}
\alias{nmModel-class}
\alias{getThetas,nmModel-method}
\alias{getSigmas,nmModel-method}
\alias{getOmegas,nmModel-method}

\title{Auxilliary class for representing control file statements}
\description{Auxilliary class for representing control file statements}

\section{Objects from the Class}{
Objects of this class inherit from the \code{\link{list}} class .
}

\section{Extends}{
Class \code{\link{list}}, directly.
}

\section{Methods}{
  \describe{
    \item{getOmegas}{\code{signature(obj = "nmModel")}: Retrieves the omega estimates from a particular problem}
    \item{getSigmas}{\code{signature(obj = "nmModel")}: Retrieves the sigma estimates from a particular problem}
    \item{getThetas}{\code{signature(obj = "nmModel")}: Retrieves the theta estimates from a particular problem}
	}
}

\author{Mango Solutions <support@mango-solutions.com>}

\examples{
	 showClass("nmModel")
}
\keyword{classes}
