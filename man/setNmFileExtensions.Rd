\name{setNmFileExtensions}
\alias{setNmFileExtensions}
\title{file allowed file extensions}
\usage{setNmFileExtensions(parameter, extension)
}
\description{Sets the allowed file extensions for various NONMEM file types.  These are control, report, output tables, and 
input data tables.  When loading a file, the package will fail if its extension is not admissable.}
\value{None}
\author{Mango Solutions}
\keyword{environment}
\arguments{\item{parameter}{The name of the file type. These are \code{control}, \code{report}, \code{table} and \code{input}}
\item{extension}{A character vector of new file type extensions}
}
\examples{\dontrun{
setNmFileExtensions("control", c("mod", "con") )
}}

