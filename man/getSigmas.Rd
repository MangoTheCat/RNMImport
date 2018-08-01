\name{getSigmas}
\alias{getSigmas}
\title{A generic function that extracts sigma estimates (and initial estimates and standard errors if specified) from a NONMEM object.}
\usage{getSigmas(obj, what="final", subProblemNum=1, method=1, problemNum=1)
}
\description{A generic function that extracts sigma estimates (and initial estimates and standard errors if specified) from a NONMEM object.
Shrinkage values may also be retrieved for NONMEM 7 objects.}
\value{For NONMEM 6 - either a list of matrices if multiple "what=" are used, or a single matrix otherwise
For NONMEM 7 - The same as above for a single method.  If multiple methods, a list of lists or a list of matrices}
\author{Mango Solutions}
\note{Invalid \code{what} elements are simply ignored.}
\keyword{methods}
\arguments{\item{obj}{An object of class NMBasicModel, NMRun, NMSimModel, NMBasicModelNM7, NMSimModelNM7, or nmModel}
\item{what}{Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models)}
\item{subProblemNum}{Numeric vector of simulation sub-problems to use.  Only applies to simulation models}
\item{method}{Numeric vector of methods to extract when dealing with NONMEM 7 problems}
\item{problemNum}{Number of problem to reference - applies to runs only}
}
\examples{\dontrun{
x <- importNm("theoph.con", path = "examples/theoph")
getSigmas(x, what = c("initial", "final")) 
}}

