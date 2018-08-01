\name{importNmMod}
\alias{importNmMod}
\title{Parse a NONMEM control file}
\usage{importNmMod(fileName, path, version="VI", textReport=FALSE)
}
\description{Parses a NONMEM control file, and returns its contents as a list of parsed elements.  These elements
will correspond to the actual control statements, e.g. $PK, $THETA, $PROBLEM, etc.  Some of the elements
are kept as pure text.}
\value{A list with parsed components of the control file.  This will be of class \code{nmModel}.}
\author{Mango Solutions}
\note{An execption will be generated if $PRIOR NWPRI is found in a control file and NONMEM version VII was used. If the control file is empty, 
an exception will also be generated.}
\keyword{IO}
\arguments{\item{fileName}{File name name of the control file}
\item{path}{(optional) path to the control file, can be a stored path enclosed in round brackets}
\item{version}{NONMEM version that the control file targets.  Currently, "VII" or "7" denotes NONMEM 7, and
other values are not differentiated}
\item{textReport}{Logical flag.  If TRUE, a message regarding which file is being imported will be logged.}
}
\examples{\dontrun{
conContents <- importNmMod(file = "theoph.con", path = "examples/theoph") 
print(conContents)
}}

