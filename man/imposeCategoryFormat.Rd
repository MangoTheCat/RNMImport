\name{imposeCategoryFormat}
\alias{imposeCategoryFormat}
\title{Forces the data in a NONMEM object to conform to the variable formats specified in the metadata.}
\usage{imposeCategoryFormat(obj, varSubset, ...)
}
\description{Forces the data in a NONMEM object to conform to the variable formats specified in the metadata. Users will be allowed to choose a subset of 
variables which should be formatted.  Variables that are formatted should be coerced to factors, with the levels set as described by the comma 
separated string stored within the metadata.}
\value{A new object with the required data coerced to factors of the correct form}
\author{Mango Solutions}
\arguments{\item{obj}{An object of class NMRun, NMBasicModel or data.frame}
\item{varSubset}{The variables to be converted to factors. It can be a vector of variable names or a comma separated string}
\item{...}{Additional arguments that apply to different classes. These are \code{problemNum} which specifies the required run 
for a NMRun object}
}
\examples{x <- data.frame(SMOK = sample(0:1, 10, replace = TRUE), SEX = sample(1:2, 10, replace = TRUE))
print(imposeCategoryFormat(x))}

