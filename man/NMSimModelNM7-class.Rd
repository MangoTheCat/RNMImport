\name{NMSimModelNM7-class}
\docType{class}
\alias{NMSimModelNM7-class}
\alias{addDerivedCategorical,NMSimModelNM7-method}
\alias{getObjective,NMSimModelNM7-method}
\alias{getOmegas,NMSimModelNM7-method}
\alias{getSigmas,NMSimModelNM7-method}
\alias{getThetas,NMSimModelNM7-method}
\alias{getMethodNames,NMSimModelNM7-method}
\alias{getSimInfo,NMSimModelNM7-method}
\alias{imposeCategoryFormat,NMSimModelNM7-method}
\alias{nmData,NMSimModelNM7-method}
\alias{show,NMSimModelNM7-method}

\title{Class "NMSimModelNM7" }
\description{	
This class holds the results of NONMEM 7 problems that both simulate data and fit the model during each simulation
(that is, models with a $SIM statement in the control file but no "ONLY" keyword).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NMSimModel7", ...)}.  There is also an internal constructor
\code{NMSimModelNM7}, but it is not meant to be called directly.  Most of the time, instances of this class will be
created when importing a run via \code{\link{importNm}}
}
\section{Slots}{
	 \describe{
    \item{\code{numSimulations}:}{Object of class \code{"numeric"}. Number of simulations generated }
    \item{\code{objectiveFinal}:}{Object of class \code{"matrix"} Matrix of final values of the objective functions.  One dimension is for the method, one is for the simulation sub-problem. }
    \item{\code{numMethods}:}{Object of class \code{"numeric"} Number of estimation methods used }
    \item{\code{methodInfo}:}{Object of class \code{"matrix"} Partially parsed matrix of $EST information }
    \item{\code{methodNames}:}{Object of class \code{"character"} Names of the estimation methods used, }
    \item{\code{thetaFinal}:}{Object of class \code{"array"} 3 dimensional array of estimation results, with one dimension for the simulations, one for the parameters, and one for the methods}
    \item{\code{omegaFinal}:}{Object of class \code{"list"} List of 3d arrays of omega estimates (the third dimension is for the simulations) }
    \item{\code{sigmaFinal}:}{Object of class \code{"list"} List of 3d arrays of sigma estimates (the third dimension is for the simulations)  }
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
    \item{addDerivedCategorical}{\code{signature(obj = "NMSimModelNM7")}: Create user-defined categorical variables}
    \item{getMethodNames}{\code{signature(obj = "NMSimModelNM7")}: Retrieves the estimation method names}
    \item{getObjective}{\code{signature(obj = "NMSimModelNM7")}: Retrieves a set of objective function values for given methods (method) and simulation (subProblem) }
    \item{getOmegas}{\code{signature(obj = "NMSimModelNM7")}: Retrieves an array of omegas for a given method (method) and simulations (subProblemNum)}
    \item{getSigmas}{\code{signature(obj = "NMSimModelNM7")}: Retrieves an array of sigmas for a given method (method) and simulations (subProblemNum)}
    \item{getThetas}{\code{signature(obj = "NMSimModelNM7")}: Retrieves a matrix of thetas for a given method (method) and simulations (subProblemNum) }
    \item{nmData}{\code{signature(obj = "NMSimModelNM7")}: Extracts NONMEM data.  A parameter stackInput is also available.  
    	If TRUE, will cause input to be "stacked" enough times that it matches the size of the output.}
    \item{imposeCategoryFormat}{\code{signature(obj = "NMSimModelNM7")}: Coerces variables to categories}
    \item{show}{\code{signature(object = "NMSimModelNM7")}: ... }
	 }
}
\author{ Mango Solutions }

\seealso{
	\code{\linkS4class{NMSimModel}}, \code{\linkS4class{NMBasicModelNM7}}
}
\examples{
showClass("NMSimModelNM7")
}
\keyword{classes}
