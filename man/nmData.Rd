\name{nmData}
\alias{nmData}
\title{Extract input and / or output data tables from a NONMEM object}
\usage{nmData(obj, dataTypes=c("input", "output"), 
returnMode=c("singleDF", "DFList"), subset, ...)}
\description{A generic function that extracts input and output data tables from a NONMEM object into either a single consolidated
data.frame or a list.}
\value{A data.frame or a list of data.frames of data, depending on the value of 
\code{returnMode}. If a data.frame is requested and If variables are found in both input 
and output tables, then the variable from the output will appear in the result data frame
without modification. The input data will then appear with the postfix \code{.INPUT} added
to the column names. If any of the output tables generated were produced with a FIRSTONLY 
flag, these will be discarded by nmData (this should change in a future version of the 
package).}
\author{Mango Solutions}
\keyword{utilities}
\keyword{datagen}
\note{Invalid types in \code{dataTypes} will be discarded. If no valid types were chosen, an exception should be generated.}
\arguments{\item{obj}{An object of class NMRun, or one that inherits from NMProblem}
\item{dataTypes}{Which type of data should be returned, must 
be "input" and/or "output". These correspond to data for the object which correspond to either inputs (as specified in the $DATA statement)
or outputs (specified by the $TABLE statement)}
\item{returnMode}{Determines how the data should be returned.  If "singleDF", attempts to return a consolidated data.frame of output
and input data, if "DFList" returns a list with seperate input and output data}
\item{...}{Additional parameters: problemNum to select the problem if obj is of class NMRun, subProblemNum to select a 
set of subproblems for simulation problems}
\item{subset}{Controls the application of a "subset" to the data when it is returned. In general this is used to remove dosing records from the data. If NULL or FALSE, nothing is done. 
Otherwise, it should be a character vector of subsetting expressions (such as "MDV = 1", etc.) which will be applied in succession, OR
it may be \code{TRUE}, in which case the associated data subset of the object (retrieved by \code{\link{dataSubset}}) will be applied.}
}
\examples{\dontrun{
x <- importNm("TestData1.ctl", "TestData1.lst", 
  path = system.file(package = "RNMImport", "testing/testdata/TestRun")
x.data <- nmData(x)
print(head(x.data))
x.data <- nmData(x, returnMode = "DFList" )
print(lapply(x.data, head))
y <- importNm("TestData1SIM.con", "TestData1SIM.lst", 
  path = "testing/testdata/TestSimRun")
y.outputData <- nmData(y, subProblemNum = 2:4, dataTypes = "output")
print(summary(y.outputData))
}}

