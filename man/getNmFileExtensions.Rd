\name{getNmFileExtensions}
\alias{getNmFileExtensions}
\title{Gets allowed NONMEM file extensions}
\usage{getNmFileExtensions(parameter)
}
\description{Returns a vector of allowed file extensions for a specific NONMEM file type, e.g. control, report, output table or 
input data.Gets the conventional file name extensions used by NONMEM.}
\value{A vector of allowed file type extensions}
\author{Mango Solutions}
\keyword{environment}
\arguments{\item{parameter}{String with the file type. Must be one of "control", "report", "table" or "input"}
}
\examples{getNmFileExtensions("control")}

