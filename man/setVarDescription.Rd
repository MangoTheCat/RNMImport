\name{setVarDescription}
\alias{setVarDescription}
\title{Sets new variable characteristics}
\description{Sets a new label, format and type for an existing variable name, e.g. one may want to change the label for SEX 
or change the format of IPRED.}
\usage{
setVarDescription(varName, varLabel = NULL, varFormat = NULL, varType = NULL)
}
\arguments{
  \item{varName}{The variable name.}
  \item{varLabel}{The new label. NULL by default}
  \item{varFormat}{The new format. NULL by default}
  \item{varType}{The new type. NULL by default}
}
\value{None}
\author{Mango Solutions <rweeks@mango-solutions.com>}
\keyword{environment}