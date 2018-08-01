\name{NMSimModel-class}
\docType{class}
\alias{NMSimModel-class}
\alias{getObjective,NMSimModel-method}
\alias{getOmegas,NMSimModel-method}
\alias{getSigmas,NMSimModel-method}
\alias{getThetas,NMSimModel-method}
\alias{nmData,NMSimModel-method}
\alias{show,NMSimModel-method}
\alias{getThetas,NMSimModel-method}
\alias{getSigmas,NMSimModel-method}
\alias{getOmegas,NMSimModel-method}
\alias{getSimInfo,NMSimModel-method}
\alias{addDerivedCategorical,NMSimModel-method}
\alias{imposeCategoryFormat,NMSimModel-method}

\title{Class "NMSimModel" }
\description{This class holds the results of NONMEM problems that both simulate data and fit the model during each simulation
(that is, models with a $SIM statement in the control file but no "ONLY" keyword).}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NMSimModel", ...)}.  There is also an internal constructor
\code{NMSimModel}, but it is not meant to be called directly.  Most of the time, instances of this class will be
created when importing a run via \code{\link{importNm}}. 
}
\section{Slots}{
	 \describe{
    \item{\code{numSimulations}:}{Object of class \code{"numeric"}. Number of simulations generated }
    \item{\code{thetaFinal}:}{Object of class \code{"matrix"}. Final estimates of the "thetas", stored as a matrix with one row for each simulation }
    \item{\code{objectiveFinal}:}{Object of class \code{"numeric"}. Vector of final values of the objective functions.}
    \item{\code{omegaFinal}:}{Object of class \code{"array"}. Final estimates of the "omegas", stored as a 3-d array with one matrix for each simulation.}
    \item{\code{sigmaFinal}:}{Object of class \code{"array"}. Final estimates of the "sigmas", stored as a 3-d array with one matrix for each simulation.}
    \item{\code{thetaInitial}:}{Object of class \code{"vector"}. Initial values of thetas }
    \item{\code{omegaInitial}:}{Object of class \code{"matrix"}. Initial values of omegas }
    \item{\code{sigmaInitial}:}{Object of class \code{"matrix"}. Initial values of sigmas }
    \item{\code{seeds}:}{Object of class \code{"numeric"}. Values of seeds used for random-number generation}
	}
}
\section{Extends}{
Class \code{"\linkS4class{NMProblem}"}, directly.
}
\section{Methods}{
  \describe{
    \item{getObjective}{\code{signature(obj = "NMSimModel")}: ... }
    \item{getOmegas}{\code{signature(obj = "NMSimModel")}: Retrieves the omega estimates for the simulation number}
    \item{getSigmas}{\code{signature(obj = "NMSimModel")}: Retrieves the sigma estimates for the simulation number}
    \item{getThetas}{\code{signature(obj = "NMSimModel")}: Retrieves the theta estimates for the simulation number}
    \item{addDerivedCategorical}{\code{signature(obj = "NMSimModel")}: Create user-defined categorical variables}
    \item{imposeCategoryFormat}{\code{signature(obj = "NMSimModel")}: Coerces variables to categories}
    \item{nmData}{\code{signature(obj = "NMSimModel")}: Extracts NONMEM data.  A parameter stackInput is also available.  
    	If TRUE, will cause input to be "stacked" enough times that it matches the size of the output.}
    \item{show}{\code{signature(object = "NMSimModel")}: ... }
    \item{getSimInfo}{\code{signature(obj = "NMSimModel")}: Get Simulation Information}
	 }
}

\author{Mango Solutions}

\examples{
	showClass("NMSimModel")
}
\keyword{classes}
