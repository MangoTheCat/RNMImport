\name{addedData}
\alias{addedData}
\title{Get added data}
\usage{addedData(obj, ...)
}
\description{Obtain the derived/user-added variables from a NONMEM problem object}
\value{A data.frame containing the derived variables}
\author{Mango Solutions}
\keyword{utils}
\arguments{\item{obj}{An object of class NMRun or extending NMProblem}
\item{...}{problemNum for addedData.NMRun}
}
\examples{x <- importNm("theoph.con", path = system.file("examples/theoph", package = "RNMImport") ) 
y <- getProblem(x)
y <- addDerivedCategorical(y, "DV", breaks = 3, labels = c("low", "medium", "high"))
y <- addDerivedCategorical(y, "TIME", breaks = 6)
print(addedData(y))}

