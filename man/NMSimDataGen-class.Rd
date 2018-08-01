\name{NMSimDataGen-class}
\docType{class}
\alias{NMSimDataGen-class}
\alias{nmData,NMSimDataGen-method}
\alias{show,NMSimDataGen-method}
\alias{addDerivedCategorical,NMSimDataGen-method}
\alias{imposeCategoryFormat,NMSimDataGen-method}
\alias{getOmegas,NMSimDataGen-method}
\alias{getSigmas,NMSimDataGen-method}
\alias{getThetas,NMSimDataGen-method}
\alias{getSimInfo,NMSimDataGen-method}

\title{Class for holding simulation problems that don't fit models}
\description{This class handles NONMEM models that have a $SIM statement but only simulate data without fitting a mode
}
\section{Objects from the Class}{
	Objects of this class are not usually instantiated on their own, but are created as part of a call
	to \code{importNm}.
}
\section{Slots}{
	 \describe{
    \item{\code{numSimulations}:}{Number of simulations/subproblems }
    \item{\code{seeds}:}{Seeds used for the random number generator }
    \item{\code{thetaInitial}:}{thetaInitial Fixed/initial theta values used to generate data (numeric vector)}
    \item{\code{omegaInitial}:}{Fixed/initial omega values used to generate data (numeric matrix)}
    \item{\code{sigmaInitial}:}{Fixed/initial sigma values used to generate data (numeric matrix)}
  }
}
\section{Extends}{
Virtual class \code{"\linkS4class{NMProblem}"}, directly.
}
\section{Methods}{
  \describe{
  	\item{getThetas}{\code{signature(obj = "NMSimDataGen")}: Retrieves initial theta values }
  	\item{getSigmas}{\code{signature(obj = "NMSimDataGen")}: Retrieves initial sigma values }
  	\item{getOmegas}{\code{signature(obj = "NMSimDataGen")}: Retrieves initial omega values }
  	\item{addDerivedCategorical}{\code{signature(obj = "NMSimDataGen")}: Create user-defined categorical variables}
    \item{imposeCategoryFormat}{\code{signature(obj = "NMSimDataGen")}: Coerces variables to categories}
    \item{nmData}{\code{signature(object = "NMSimDataGen")}: Retrieves the input/output data, where the output can come from 1 or more subproblems. 
     A parameter stackInput is also available. If TRUE, will cause input to be "stacked" enough times that it matches the size of the output.}
    \item{show}{\code{signature(object = "NMSimDataGen")}: Prints basic data about the problem }
    \item{getSimInfo}{\code{signature(obj = "NMSimDataGen")}: Get Simulation Information}
	 }
}

\author{Mango Solutions <support@mango-solutions.com> }

\examples{
	showClass("NMSimDataGen")
}
\keyword{classes}
