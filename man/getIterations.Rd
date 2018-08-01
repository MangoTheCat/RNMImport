\name{getIterations}
\alias{getIterations}
\title{Extract parameter estimate iteration information from a given NONMEM problem}
\usage{getIterations(obj, problemNum=1)
}
\description{This generic routine extracts the parameter estimate iteration information of a NONMEM problem, 
as produced within the report file or NONMEM 7 iteration file as applicable.}
\alias{getIterations,NMRun-method}
\alias{getIterations,NMBasicModel-method}
\alias{getIterations,NMBasicModelNM7-method}
\value{If an object produced by NONMEM <= 6 was selected, a data.frame of parsed information from the 
report file.  For NONMEM 7, a list generated from the iteration files. The contents of each file are imported
and then aggregated into a single list split by estimation method.}
\author{Mango Solutions}
\keyword{utilities}
\arguments{\item{obj}{Object of class NMRun, NMBasicModel, or NMBasicModelNM7}
\item{problemNum}{Number of the problem of NONMEM object}
}

