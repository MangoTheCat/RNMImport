
#' Retrieves the variance-covariance matrix of the estimators and optionally
#'  the correlation and inverse correlation matrices of the selected NONMEM run
#' @title Returns variance-covariance matrix, if available, from a NONMEM object 
#' @param obj An object of class NMRun or NMBasicModel
#' @param corMatrix TRUE or FALSE.  If TRUE, returns correlation matrix as well
#' @param invCorMatrix TRUE or FALSE, Not implemented yet
#' @param pdMatrix TRUE or FALSE.  If TRUE, will check that a positive-definite covariance matrix is available.  
#' If it is not, it will create one based on sample variances and covariances of estimates.  Not implemented yet
#' @param ... 
#' @return A matrix if just the covariance matrix is required, a list of matrices otherwise
#' @author Mango Solutions
#' @export

getEstimateCov <- function(obj, corMatrix = FALSE, invCorMatrix = FALSE, pdMatrix = FALSE, ...)
{
	RNMImportStop("getEstimateCov not implemented for this class yet\n", match.call())
}

setGeneric("getEstimateCov")

getEstimateCov.NMRun <- function(obj, corMatrix = FALSE, invCorMatrix = FALSE, pdMatrix = FALSE,  
		problemNum = 1, method = 1)	
{
	getEstimateCov(getProblem(obj, problemNum), corMatrix = corMatrix, method = method)
}

#' @rdname getEstimateCov
#' @export

setMethod("getEstimateCov", signature(obj = "NMRun"), getEstimateCov.NMRun)

getEstimateCov.NMBasicModel <- function(obj, corMatrix = FALSE, invCorMatrix = FALSE, pdMatrix = FALSE , ...
)
{
	
	parameterCovMatrix <- obj@parameterCovMatrix
	# check for missing variance-covariance matrix
	if(all(dim(parameterCovMatrix)) == 0)
	{
#		RNMImportWarning( "Covariance matrix not available, returning NULL\n" )
		return(NULL)
		
	}
	if(!corMatrix | all(dim(obj@parameterCorMatrix) == 0)) obj@parameterCovMatrix
	else list("covariance" = obj@parameterCovMatrix, "correlation" = obj@parameterCorMatrix)
	
}

#' @rdname getEstimateCov
#' @export

setMethod("getEstimateCov", signature(obj = "NMBasicModel"), getEstimateCov.NMBasicModel)

getEstimateCov.NMBasicModelNM7 <- function(obj, corMatrix = FALSE, invCorMatrix = FALSE, pdMatrix = FALSE , method = 1)
{
	methodChosen <- .selectMethod(obj@methodNames, method)
	parameterCovMatrix <- obj@parameterCovMatrices[[methodChosen]]
	parameterCorMatrix <- obj@parameterCorMatrices[[methodChosen]]
	# check for missing variance-covariance matrix
	if(is.null(parameterCovMatrix))
	{
#		RNMImportWarning( "Covariance matrix not available, returning NULL\n" )
		return(NULL)
		
	}
	
	if(!corMatrix | is.null(parameterCorMatrix)) parameterCovMatrix
	else list("covariance" = parameterCovMatrix, "correlation" = parameterCorMatrix )
	
}

setMethod("getEstimateCov", signature(obj = "NMBasicModelNM7"), getEstimateCov.NMBasicModelNM7)


#' getObjective will access the final value(s) of the objective function. It is a generic function, with inputs/outputs on different classes described below:  
#' @param obj NMRun, or problem inheriting from NMProblem 
#' @param addMinInfo Logical flag.  Should the minimization info be added if it's available?
#' @param ... Additional parameters passed to other methods. \code{problemNum} for NMRun objects 
#' \code{subProblems} for NMSim* objects, and \code{method} (numeric vector) for NM7 objects  
#' @title Retrieve objective function value
#' @return For x of class \code{NMBasicModel}, a numeric vector with minimization information attached as an attribute.
#' If x is of class \code{NMBasicModelNM7}, a vector with one entry for each method requested, For x of class \code{NMSimModel}, 
#' a numeric vector with one element for each subproblem chosen. If x is of class \code{NMSimModelNM7}, a vector with each 
#' selected subproblem number when only a single method is chosen, or a matrix with a column for each method and row for 
#' each sub-problem if multiple methods are selected. If x is of class \code{NMRun}, the output will vary according to the problem selected (as above). 
#' @author Mango Solutions
#' @keywords methods
#' @examples
#' \dontrun{
#'      x <- importNm("theoph.con", path  = "examples/theoph")
#'      getObjective(x)
#' }

getObjective <- function(obj, addMinInfo = TRUE, ...)
{
	RNMImportStop("getObjective not implemented for this class type")
}

setGeneric("getObjective")

getObjective.NMRun <- function(obj, addMinInfo=TRUE, subProblems=1, problemNum=1, method = 1)
{
	getObjective(getProblem(obj, problemNum), addMinInfo, subProblems, method = method)
}

#' @rdname getObjective
#' @export

setMethod("getObjective", signature(obj="NMRun"), getObjective.NMRun)

getObjective.NMBasicModel <- function(obj, addMinInfo=TRUE, ...)
{
	objective <- obj@objectiveFinal
	if(addMinInfo)
	{
		if(is.null(obj@minInfo))
			RNMImportWarning("Minimization data is missing, will not return\n")
		else
			attr(objective, "minInfo") <- obj@minInfo	
	}
	objective
}

#' @rdname getObjective
#' @export

setMethod("getObjective", signature(obj="NMBasicModel"), getObjective.NMBasicModel)

getObjective.NMBasicModelNM7 <- function(obj, addMinInfo=TRUE, method = 1, ...)
{
	
	methodsChosen <- intersect(method, seq_along(obj@methodNames))

	if(length(methodsChosen) == 0)
		RNMImportStop("No valid method chosen!", call = match.call() )
	
	objective <- obj@objectiveFinal[methodsChosen]
	
	if(addMinInfo)
	{
		if(length(obj@minInfo) == 0)
			RNMImportWarning("Minimization data is missing, will not return\n")
		else
		{
			attr(objective, "minInfo") <- obj@minInfo[methodsChosen]	
			RNMImportWarning("Only first line of minimization information is available for NONMEM 7 objects")
		}
	}
	
	objective
}

setMethod("getObjective", signature(obj="NMBasicModelNM7"), getObjective.NMBasicModelNM7)


getObjective.NMSimModel <- function(obj, addMinInfo = TRUE, subProblems = 1,...)
{	
	
	obj@objectiveFinal[subProblems]
}

#' @rdname getObjective
#' @export

setMethod("getObjective", signature(obj="NMSimModel"), getObjective.NMSimModel)


getObjective.NMSimModelNM7 <- function(obj, addMinInfo = TRUE, subProblems = 1, method = 1, ...)
{	
	
	methodsChosen <- intersect(method, seq_along(obj@methodNames))
	
	if(length(methodsChosen) == 0)
		RNMImportStop("No valid method chosen!", call = match.call() )
	
	objective <- obj@objectiveFinal[subProblems, methodsChosen]
	objective
}

#' @rdname getObjective
#' @export

setMethod("getObjective", signature(obj="NMSimModelNM7"), getObjective.NMSimModelNM7)


#' Retrieves information about a run's control and report files as a data.frame
#' @param run Object of class NMRun
#' @title Retrieve file information 
#' @return A data.frame with 2 rows, 1 describing the report file and the other the control file
#' @author Mango Solutions
#' @keywords utility
#' @export

getFileinfo <- function(run)
{
	assertClass(run, "NMRun")
	rbind("controlFile" = run@controlFileInfo, "reportFile" = run@reportFileInfo)
}

#' @rdname getFileinfo
#' @export

getReporttext <- function(run) 	run@reportText

#' @rdname getFileinfo
#' @export

getControltext <- function(run) run@controlText


#' Extracts the list of "parsed control statements" associated with a problem or run 
#' @param obj NMRun or NMProblem object
#' @param ... Various arguments specific to methods
#' @title Extract list of parsed control statement
#' @return A list with the parsed control statements (e.g. an actual matrix for the Thetas, etc.,
#' @author Mango Solutions
#' @export
#' 

getControlStatements <- function(obj, ...)
{
	RNMImportStop("This function is not implemented for objects of this class")
}

setGeneric("getControlStatements")

getControlStatements.NMRun <- function(obj, problemNum = 1)
{
	getControlStatements(getProblem(obj, problemNum))
}

#' @rdname getControlStatements
#' @export

setMethod("getControlStatements", signature(obj = "NMRun"), getControlStatements.NMRun)

getControlStatements.NMProblem <- function(obj, ...) 
{
	obj@controlStatements
}

#' @rdname getControlStatements
#' @export

setMethod("getControlStatements", signature(obj = "NMProblem"), getControlStatements.NMProblem)

#' Gets the version of NONMEM used to create compile obj
#' @param obj NMProblem or NMRun
#' @return A named character vector with entries "major" and "minor", corresponding to the major and minor NONMEM 
#' version
#' @author Mango Solutions
#' @export
#'

getNmVersion <- function(obj)
{
	RNMImportStop("This function is not implemented for objects of this class")
}

setGeneric("getNmVersion")

getNmVersion.NMRunProb <- function(obj)
{
	versionInfo <- c(obj@nmVersionMajor, as.character(obj@nmVersionMinor))
	names(versionInfo) <- c("major", "minor")
	versionInfo
}

#' @rdname getNmVersion
#' @export

setMethod("getNmVersion", signature(obj = "NMRun"), getNmVersion.NMRunProb)

#' @rdname getNmVersion
#' @export

setMethod("getNmVersion", signature(obj = "NMProblem"), getNmVersion.NMRunProb)

#' Extract information simulation information from a simulation problem
#' @param obj Object of class NMRun, or NMSim*
#' @param problemNum Number of problem (applicable only when obj is of class NMRun)
#' @param addRawInfo Should the raw contents of the $SIM statement be added to the returned information?
#' @title Get Simulation Information
#' @return A numeric vector with 3 entries : numSimulations, seed1, and seed2.  These are self-explanatory.
#' If addRawInfo is TRUE, the returned result will have an attribute named "rawStatement" that will have the text
#' of the control stream $SIM field.
#' @author Mango Solutions
#' @keywords classes, manip, utilities
#' @export
#'

getSimInfo <- function(obj, problemNum = 1, addRawInfo = TRUE)
{
	RNMImportStop("This function is not implemented for objects of this class")
}

setGeneric("getSimInfo")

getSimInfo.NMRun <- function(obj, problemNum = 1, addRawInfo = TRUE)
{
	getSimInfo(getProblem(obj, problemNumber = problemNum), addRawInfo = addRawInfo)
}

#' @rdname getSimInfo
#' @export

setMethod("getSimInfo", signature(obj = "NMRun"), getSimInfo.NMRun)

getSimInfo.NMSim <- function(obj, problemNum = 1, addRawInfo = TRUE)
{
	# extract the aprsed contgrol statments
	
	controlStatements <- obj@controlStatements
	
	# the raw $SIM statement is an attribute of the Sim element
	
	rawSim <- attr(controlStatements$Sim, "rawStatement")
	simInfo <- c(obj@numSimulations, obj@seeds)
	names(simInfo) <- c( "numSimulations", "seed1", "seed2" )
	if(addRawInfo) attr(simInfo, "rawStatement") <- rawSim
	simInfo
}

#' @rdname getSimInfo
#' @export

setMethod("getSimInfo", signature(obj = "NMSimModelNM7"), getSimInfo.NMSim)

#' @rdname getSimInfo
#' @export

setMethod("getSimInfo", signature(obj = "NMSimDataGen"), getSimInfo.NMSim)

#' @rdname getSimInfo
#' @export

setMethod("getSimInfo", signature(obj = "NMSimModel"), getSimInfo.NMSim)
