\name{importNmInter}
\alias{importNmInter}

\title{Import NONMEM "inter" file}
\description{
  Imports a NONMEM "INTER" file into a data.frame with one row for each iteration
}
\usage{
importNmInter(fileName, path = NULL)
}
\arguments{
  \item{fileName}{ Name of the "Inter" file to read}
  \item{path}{ Path that holds the file}
}
\value{
	A data.frame with the iteration information
}

\author{Mango solutions}

\keyword{IO}

