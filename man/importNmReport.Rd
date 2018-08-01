\name{importNmReport}
\alias{importNmReport}
\title{Import the information in an output report file}
\usage{importNmReport(fileName, path, controlStatements, textReport=FALSE)
}
\description{This routine imports the contents in a NONMEM output report file, and then parses different sections of it
while ignoring some. A structured list object is returned}
\value{A named report with elements holding parsed statements in the report file.
This object will be of class \code{nmRunReport}}
\author{Mango Solutions}
\keyword{IO}
\arguments{\item{fileName}{Name of the report file}
\item{path}{The path where the report file is held. Can be a path stored by setNmPath if it is in round brackets.}
\item{controlStatements}{(optional) Obh Control statements from the run associated with this output.  Using it 
allows cleaner parsing of the report file.  This will likely be deprecated in the future.}
\item{textReport}{Single logical.  Should statements be logged to "stdReport" log?}
}
\examples{\dontrun{
x <- importNmReport("theoph.lst", path = "examples/theoph")
print(x)
}}

