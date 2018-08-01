\name{getOmegas}
\alias{getOmegas}
\title{Retrieve OMEGA estimates, initial values of}
\usage{getOmegas(obj, what="final", subProblemNum=1, method=1, problemNum=1)
}
\description{A generic function that extracts omega estimates (and initial estimates and standard errors if specified) from a NONMEM object.
For NONMEM 7 objects, shrinkage values will also be available}
\value{For NONMEM 6 - either a list of matrices if multiple 
"what=" are used, or a single matrix otherwise For NONMEM 7 - The same as above for a single method.
If multiple methods, a list of lists or a list of matrices}
\author{Mango Solutions}
\note{Invalid \code{what} elements are simply ignored.}
\arguments{\item{obj}{An object of class NMBasicModel, NMRun, NMSimModel, NMBasicModelNM7, NMSimModelNM7, or nmModel}
\item{what}{Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models)}
\item{subProblemNum}{[N,+] Numeric vector of simulation sub-problems to use.  Only applies to simulation models}
\item{method}{[N,+] Vector of methods to extract when dealing with NONMEM 7 problems}
\item{problemNum}{[N,1] Number of problem to reference - applies to runs only}
}
\examples{\dontrun{
x <- importNm("theoph.con", path = "examples/theoph")
getOmegas(x, what = c("initial", "final")) 
}}

