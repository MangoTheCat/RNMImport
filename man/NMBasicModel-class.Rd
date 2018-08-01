\name{NMBasicModel-class}
\docType{class}
\alias{NMBasicModel-class}
\alias{getThetas,NMBasicModel-method}
\alias{nmData,NMBasicModel-method}
\alias{show,NMBasicModel-method}
\alias{getObjective,NMBasicModel-method}
\alias{getEstimateCov,NMBasicModel-method}
\alias{getThetas,NMBasicModel-method}
\alias{getSigmas,NMBasicModel-method}
\alias{getOmegas,NMBasicModel-method}
\alias{addDerivedCategorical,NMBasicModel-method}
\alias{imposeCategoryFormat,NMBasicModel-method}


\title{Standard/"basic" NONMEM individual problem}
\description{ This class holds the results from an individual NONMEM problem (not an entire control file) that
has no $SIM step.}
\section{Objects from the Class}{
	Individual objects of this class are not meant to be instantiated on their own, but are created when loading
	a NONMEM run via \code{importNm}
}
\section{Slots}{
	 \describe{
    \item{\code{parameterIterations}:}{A data.frame of the iteration of each parameter estimate, if available }
    \item{\code{objectiveFinal}:}{The numeric value of the objective function minimum}
    \item{\code{thetaInitial}:}{Initial value of the "thetas"}
    \item{\code{sigmaInitial}:}{Initial value of the "sigmas"}
    \item{\code{omegaInitial}:}{Initial value of the "omegas"}
    \item{\code{thetaFinal}:}{Final estimates of the "thetas", together with the standard errors, if available (as a 2-row matrix)}
    \item{\code{sigmaFinal}:}{Final estimates of the "sigmas", together with the standard errors, if available (as an array  with 1 or 2 matrices)}
    \item{\code{omegaFinal}:}{Final estimates of the "omegas", together with the standard errors, if available (as an array  with 1 or 2 matrices)}
    \item{\code{parameterCovMatrix}:}{The variance-covariance of the parameter estimators, if available}
    \item{\code{parameterCorMatrix}:}{The correlation matrix of the parameter estimators, if available}
    \item{\code{minInfo}:}{A string describing the status of the objective function-minimization }
  }
}
\section{Extends}{
The virtual class \code{"\linkS4class{NMProblem}"}, directly.
}
\section{Methods}{
  \describe{
    \item{getOmegas}{\code{signature(obj = "NMBasicModel")}: Retrieves the omega estimates from the problem}
    \item{getSigmas}{\code{signature(obj = "NMBasicModel")}: Retrieves the sigma estimates from the problem}
    \item{getThetas}{\code{signature(obj = "NMBasicModel")}: Retrieves the theta estimates from the problem}
    \item{addDerivedCategorical}{\code{signature(obj = "NMBasicModel")}: Create user-defined categorical variables}
    \item{imposeCategoryFormat}{\code{signature(obj = "NMBasicModel")}: Coerces variables to categories}
    \item{nmData}{Extracts the input and output data from the problem }
    \item{show}{Display overview of problem}
    \item{getObjective}{\code{signature(obj = "NMBasicModel")}}
    \item{getEstimateCov}{\code{signature(obj = "NMBasicModel")}: Extracts the covariance matrix and optionally the correlation matrix of the estimators for this problem.}
	}
}
\author{ Mango Solutions <support@mango-solutions.com> }
\keyword{classes}
