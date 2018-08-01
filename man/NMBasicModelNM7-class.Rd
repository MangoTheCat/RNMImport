\name{NMBasicModelNM7-class}
\docType{class}
\alias{NMBasicModelNM7-class}
\alias{addDerivedCategorical,NMBasicModelNM7-method}
\alias{getEstimateCov,NMBasicModelNM7-method}
\alias{getObjective,NMBasicModelNM7-method}
\alias{getOmegas,NMBasicModelNM7-method}
\alias{getSigmas,NMBasicModelNM7-method}
\alias{getThetas,NMBasicModelNM7-method}
\alias{getMethodNames,NMBasicModelNM7-method}
\alias{imposeCategoryFormat,NMBasicModelNM7-method}
\alias{nmData,NMBasicModelNM7-method}
\alias{show,NMBasicModelNM7-method}

\title{Standard/"basic" NONMEM 7 individual problem}
\description{This class holds the results from an individual NONMEM 7 problem (not an entire control file) that
has no $SIM step.)}
\section{Objects from the Class}{
	Individual objects of this class are not meant to be instantiated on their own, but are created when loading
	a NONMEM run via \code{importNm}.
}
\section{Slots}{
	 \describe{
    \item{\code{parameterIterations}:}{ Currently unused }
    \item{\code{objectiveFinal}:}{Vector of objective function final values (one for each method)}
    \item{\code{methodInfo}:}{$EST statement contents partially parsed into a matrix }
    \item{\code{methodNames}:}{character vector of full method names (from report file)}
    \item{\code{thetaInitial}:}{List of theta initial value vectors, one element for each method used}
    \item{\code{sigmaInitial}:}{List of sigma initial value vectors, one element for each method used}
    \item{\code{omegaInitial}:}{List of omega initial value vectors, one element for each method used}
    \item{\code{thetaFinal}:}{List of theta final estimate vectors, one element for each method used}
    \item{\code{sigmaFinal}:}{List of sigma final estimate vectors, one element for each method used}
    \item{\code{omegaFinal}:}{List of omega final estimate vectors, one element for each method used}
    \item{\code{thetaStderr}:}{ List of theta standard error vectors, one element for each method used }
    \item{\code{sigmaStderr}:}{ List of sigma standard error matrices, one element for each method used }
    \item{\code{omegaStderr}:}{List of omega standard error matrices, one element for each method used}
    \item{\code{ETAShrinkage}:}{ Matrix of ETA shrinkages, one row for each method }
    \item{\code{EPSShrinkage}:}{Matrix of EPS shrinkages, one row for each method }
    \item{\code{parameterCovMatrices}:}{ List of parameter covariance matrices, one for each method }
    \item{\code{parameterCorMatrices}:}{ List of parameter correlation matrices, one for each method  }
    \item{\code{minInfo}:}{Character vector holding the first line of the termination status for each method}
  }
}
\section{Extends}{
Class \code{"\linkS4class{NMProblem}"}, directly.
}
\section{Methods}{
  \describe{
    \item{addDerivedCategorical}{\code{signature(obj = "NMBasicModelNM7")}: Create user-defined categorical variables}
    \item{getMethodNames}{\code{signature(obj = "NMBasicModelNM7")}: Retrieves the estimation method names}
    \item{getObjective}{\code{signature(obj = "NMBasicModelNM7")} Extracts objective function value. }
    \item{getEstimateCov}{\code{signature(obj = "NMBasicModelNM7")}: Extracts the covariance matrix and optionally the correlation matrix of 
    the estimators for this problem.}
	\item{getOmegas}{\code{signature(obj = "NMBasicModelNM7")}: Retrieves the omega estimates from the problem and chosen method number }
    \item{getSigmas}{\code{signature(obj = "NMBasicModelNM7")}: Retrieves the sigma estimates from the problem and chosen method number}
    \item{getThetas}{\code{signature(obj = "NMBasicModelNM7")}: Retrieves the theta estimates from the problem and chosen method number }
    \item{imposeCategoryFormat}{\code{signature(obj = "NMBasicModelNM7")}: Coerces variables to categories}
    \item{nmData}{\code{signature(obj = "NMBasicModelNM7")}: Extracts the input and output data from the problem }
    \item{show}{\code{signature(object = "NMBasicModelNM7")}: ... }
	 }
}
\author{ Mango Solutions }


\seealso{
	 \code{\linkS4class{NMBasicModel}}, \code{\linkS4class{NMSimModelNM7}} 
}
\examples{
showClass("NMBasicModelNM7")
}
\keyword{classes}
