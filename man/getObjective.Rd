\name{getObjective}
\alias{getObjective}
\title{Retrieve objective function value}
\usage{getObjective(obj, addMinInfo=TRUE, ...)
}
\description{getObjective will access the final value(s) of the objective function. It is a generic function, with inputs/outputs on different classes described below:}
\value{For x of class \code{NMBasicModel}, a numeric vector with minimization information attached as an attribute.
If x is of class \code{NMBasicModelNM7}, a vector with one entry for each method requested, For x of class \code{NMSimModel}, 
a numeric vector with one element for each subproblem chosen. If x is of class \code{NMSimModelNM7}, a vector with each 
selected subproblem number when only a single method is chosen, or a matrix with a column for each method and row for 
each sub-problem if multiple methods are selected. If x is of class \code{NMRun}, the output will vary according to the problem selected (as above).}
\author{Mango Solutions}
\note{The parameters \code{method} and \code{subProblem} will be restricted to valid ranges suitable for the given object}
\keyword{methods}
\arguments{\item{obj}{NMRun, or problem inheriting from NMProblem}
\item{addMinInfo}{Logical flag.  Should the minimization info be added if it's available?}
\item{...}{Additional parameters passed to other methods. \code{problemNum} for NMRun objects 
\code{subProblems} for NMSim* objects, and \code{method} (numeric vector) for NM7 objects}
}
\examples{\dontrun{
x <- importNm("theoph.con", path  = "examples/theoph")
getObjective(x)
}}

