\name{nmDatabyVarType}
\alias{nmDatabyVarType}
\title{Get NONMEM data by variable type}
\usage{nmDatabyVarType(obj, varTypes, returnMode=c("singleDF", "DFList"), ...)
}
\description{Retrieves data stored in a NONMEM object by category of data type, e.g. covariates, 
"etas", etc.  The type mappings are defined in the configuration data, and may be changed with
get/setNmVarDescription}
\value{A data.frame, or list, with the data}
\author{Mango Solutions}
\seealso{\code{\link{nmData}}, \code{\link{getVarDescription}}}
\keyword{datagen}
\keyword{utils}
\arguments{\item{obj}{Object of class NMRun or NMProblem}
\item{varTypes}{Character vector of types to return.  Allowed types are currently "Parameter", 
"Lab covariate", "Covariate" and "Eta"}
\item{returnMode}{A string, either "singleDF" or "DFList".  If "singleDF", all of the data
is returned in a consolidated data.frame, otherwise as a list returned by type}
\item{...}{Additional parameters: problemNum to select the problem if obj is of class NMRun, subProblemNum to select a 
set of subproblems for simulation problems}
}
\examples{\dontrun{
x <- importNm("TestData1.ctl", "TestData1.lst", path  = "testing/testdata/TestRun"))
x.covariates <- nmDatabyVarType(x, varTypes = "Covariate")
print(head(x.covariates))
}}

