\name{NMProblem-class}
\docType{class}
\alias{NMProblem-class}
\alias{nmDatabyVarType,NMProblem-method}
\alias{addedData,NMProblem-method}
\alias{getControlStatements,NMProblem-method}
\alias{getNmVersion,NMProblem-method}

\title{Class "NMProblem" }
\description{This is an abstract class that holds the common features of all 3 NONMEM problem types 
	(NMBasicModel, NMSimModel, NMSimDataGen) }
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{problemStatement}:}{Object of class \code{"character"}. String holding the contents of the 
    $PROB statement.}
    \item{\code{controlStatements}:}{Object of class \code{"list"}. List of parsed control file contents.}
    \item{\code{reportStatements}:}{Object of class \code{"list"}. List of parsed report file contents.}
    \item{\code{nmVersionMajor}:}{Object of class \code{"character"}. The major version.}
    \item{\code{nmVersionMinor}:}{Object of class \code{"character"}. The minor version.}
    \item{\code{inputData}:}{Object of class \code{"data.frame"}. Table of input data, if available.}
    \item{\code{outputData}:}{Object of class \code{"ANY"}. Consolidated table of output data, if available.}
    \item{\code{additionalVars}:}{Object of class \code{"data.frame"}. Additional user-created variables.}
  }
}

\section{Methods}{
  \describe{
   	\item{nmDatabyVarType}{\code{signature(obj = "NMProblem")}: } 
	\item{addedData}{\code{signature(obj = "NMProblem")}:  Extracts user-created data (typically through) 
	 \code{addDerivedCategorical} into a data.frame}
	 \item{getControlStatements}{\code{signature(obj = "NMProblem")}: Extract an object with parsed statements control file
		corresponding to this problem}
	\item{getNmVersion}{\code{signature(obj = "NMProblem")}: Extract information about NONMEM version used for compiling problem}
	}
}

\author{Mango Solutions}
\keyword{classes}
