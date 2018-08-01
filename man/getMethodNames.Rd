\name{getMethodNames}
\alias{getMethodNames}
\title{Get method names}
\usage{getMethodNames(obj, what=c("report", "control"), problemNum=1)
}
\description{Extracts estimation method names from a run or problem that used NONMEM 7.  These
can be the names used in the control file, or the report file}
\value{A character vector of method names used}
\note{If what = "control", the names are taken from the $EST control sections}
\author{Mango Solutions}
\keyword{methods}
\keyword{utilities}
\arguments{\item{obj}{NMRun, NMBasicModelNM7, or NMSimModelNM7}
\item{what}{The string "report" or "control" - selects which file should be used
for the method names}
\item{problemNum}{Problem number in run}
}
\examples{\dontrun{
x <- importNm("TestData1.ctl", "TestData1.lst", path  = "testing/testdata/TestDataNM7")
print(getMethodNames(x))
}}

