\name{readNmData}
\alias{readNmData}
\title{Read NONMEM dataset removing any "IGNORE" rows and dealing with (possibly repeated) headers...}
\usage{readNmData(file, ignore, accept, translate, records, null=NA, sep)
}
\description{Read NONMEM dataset removing any "IGNORE" rows and dealing with (possibly repeated) headers 
This function is designed to import either output table or raw input data
The ignore string can optionally be added to remove rows from the data
Any table file headers found are removed from the data - repeated headers are also removed}
\value{A data.frame of the contents of the tables in the file}
\note{At the moment, there is an issue with the "ignore" field, in that it does not handle multiple ignore
characters.  This will be dealt with in future releases.}
\keyword{IO}
\author{Mango Solutions}
\arguments{\item{file}{Name of the file}
\item{ignore}{[C,1] - ignore option, see NM?$DATA[IGNORE]}
\item{accept}{[C,1] - accept option, see NM?$DATA[ACCEPT]}
\item{translate}{[C,1] - translate option, see NM?$DATA[TRANSLATE]}
\item{records}{[CN,1] - number of records, see NM?$DATA[RECORDS]}
\item{null}{[C,1] - value to replace NA, see NM?$DATA[NULL]}
\item{sep}{character that seperates fields}
}
\examples{\dontrun{ 
inData <- readNmData(file =  "examples/theoph/data.csv") 
print(head(inData))
outData <- readNmData(file = "examples/theoph/Outtable" )
print(head(outData))
}}

