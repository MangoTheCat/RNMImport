\name{NMRun-class}
\docType{class}
\alias{NMRun-class}
\alias{getThetas,NMRun-method}
\alias{nmData,NMRun-method}
\alias{show,NMRun-method}
\alias{getEstimateCov,NMRun-method}
\alias{getObjective,NMRun-method}
\alias{nmDatabyVarType,NMRun-method}
\alias{addedData,NMRun-method}
\alias{getThetas,NMRun-method}
\alias{getSigmas,NMRun-method}
\alias{getOmegas,NMRun-method}
\alias{getMethodNames,NMRun-method}
\alias{addDerivedCategorical,NMRun-method}
\alias{imposeCategoryFormat,NMRun-method}
\alias{getControlStatements,NMRun-method}
\alias{getNmVersion,NMRun-method}
\alias{getSimInfo,NMRun-method}

\title{NONMEM run container class}
\description{This is the basic class for handling a standard NONMEM run.  In essence, it holds
several individual problems which may be of type NMBasicModel, NMSimModel, and NMSimDataGen.
It also holds the control file and list output file text }

\section{Objects from the Class}{
Objects of this class are typically created with the importNm function, which
imports a NONMEM run on the basis of its control file.
}
\section{Slots}{
	 \describe{
    \item{\code{controlText}:}{Text of the control file, without comments}
    \item{\code{reportText}:}{Text of the report file}
    \item{\code{nmVersionMajor}:}{Major version of NONMEM}
    \item{\code{nmVersionMinor}:}{Minor version of NONMEM}
    \item{\code{controlComments}:}{Comments of each line of the control file}
    \item{\code{controlFileInfo}:}{Information about the control file}
    \item{\code{reportFileInfo}:}{Information about the list file}
    \item{\code{numProblems}:}{Number of problems in the run}
    \item{\code{problems}:}{List of the actual problem results  }
  }
}

\section{Methods}{
  \describe{
    \item{getMethodNames}{\code{signature(obj = "NMRun")}: Retrieves the estimation method names}
    \item{getOmegas}{\code{signature(obj = "NMRun")}: Retrieves the omega estimates from a particular problem}
    \item{getSigmas}{\code{signature(obj = "NMRun")}: Retrieves the sigma estimates from a particular problem}
    \item{getThetas}{\code{signature(obj = "NMRun")}: Retrieves the theta estimates from a particular problem}
   	\item{addDerivedCategorical}{\code{signature(obj = "NMRun")}: Create user-defined categorical variables}
    \item{imposeCategoryFormat}{\code{signature(obj = "NMRun")}: Coerces variables to categories}
    \item{nmData}{Extracts the input and/or output data of one of the problems 
    	in the run.}
    \item{show}{Prints basic information about the run.}
	\item{getEstimateCov}{\code{signature(obj = "NMRun")}: ... }
	\item{addedData}{\code{signature(obj = "NMRun")}:  Extracts user-created data (typically through) 
	 \code{addDerivedCategorical} into a data.frame from a specified  problem. }
	\item{getObjective}{\code{signature(obj = "NMRun")} }
	\item{getControlStatements}{\code{signature(obj = "NMRun")}: Extract an object with parsed statements control file
	corresponding to a particular problem}
	\item{getNmVersion}{\code{signature(obj = "NMRun")}: Extract information about NONMEM version used for compiling problem}
	\item{getSimInfo}{\code{signature(obj = "NMRun")}: Get Simulation Information}
	}
}

\author{Mango Solutions <support@mango-solutions.com>}

\examples{
	 showClass("NMRun")
}
\keyword{classes}
