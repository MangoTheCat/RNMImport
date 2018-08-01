\name{importNm7Tables}
\alias{importNm7Tables}
\title{Import new NONMEM 7 table files}
\usage{importNm7Tables(file, path=".", type=c("cov", "ext", "phi"), tableTitles=TRUE)
}
\description{Imports one of the new files produced by NONMEM 7: .phi, .cor, .cov, etc. into a  list of data.frames.}
\value{a list of data.frames holding the content of the individual sub-tables within each file.  Each
table will have as an attribute "method", holding the name of the method associated to it, if it was available (e.g. not if
tableTitles = FALSE)}
\author{Mango Solutions}
\note{The "cov" type covers .cor, .cov and .coi files.}
\keyword{IO}
\arguments{\item{file}{Name of the file}
\item{path}{Path to the file (can be a NONMEM path in round brackets)}
\item{type}{Type of the file: One of "cov", "ext", or "phi".  "cov" should be used for .cor, .cov or .coi files.}
\item{tableTitles}{Single logical flag. Are the table titles present in the file?  Should be FALSE, if, for example, one generates an EXT
file using the NOTITLE=1 option.  Currently this option ONLY works when type = "ext"}
}
\examples{\dontrun{ 
phiTables <- importNm7Tables(type = "phi", "TestData1.phi", path = "testing/testdata/TestDataNM7")
print(phiTables[[1]])
print(attr(phiTables[[1]], "method"))
}}

