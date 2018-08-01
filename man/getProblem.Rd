\name{getProblem}
\alias{getProblem}
\title{Extract an individual NONMEM problem}
\usage{getProblem(run, problemNumber=1)
}
\description{Retrieves a desired problem from a full NONMEM run object.}
\value{The given problem number (NMBasicModel, NMSimDataGen, or NMSimModel)}
\author{Mango Solutions}
\note{If \code{problemNum} is out of the acceptable range of problems, an exception will be generated.  The same
applies if \code{run} is not an NMRun object}
\keyword{manip}
\arguments{\item{run}{An NMRun class object}
\item{problemNumber}{Number of the problem to retrieve}
}

