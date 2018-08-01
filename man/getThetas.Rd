\name{getThetas}
\alias{getThetas}
\title{A generic function that extracts theta estimates (and initial estimates and standard errors if specified) from a NONMEM object.}
\usage{getThetas(obj, what="final", subProblemNum=1, method=1, problemNum=1)
}
\description{A generic function that extracts theta estimates (and initial estimates and standard errors if specified) from a NONMEM object.}
\value{A matrix of named rows for final estimates, initial estimates, standard errors etc. as applicable, or a list
of matrices if multiple methods are chosen in NONMEM 7}
\author{Mango Solutions}
\note{Invalid \code{what} elements are simply ignored.}
\keyword{methods}
\arguments{\item{obj}{An object of class NMBasicModel, NMRun, NMSimModel, NMBasicModelNM7, NMSimModelNM7, or  nmModel}
\item{what}{Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models)}
\item{subProblemNum}{Numeric vector of simulation sub-problems to use.  Only applies to simulation models}
\item{method}{Vector of methods to extract when dealing with NONMEM 7 problems}
\item{problemNum}{Number of problem to use - applies to NMRun only}
}
\examples{\dontrun{
x <- importNm("theoph.con", path = "examples/theoph")
getThetas(x, what = c("initial", "final")) 
}}

