

validity.NMProblem <- function(object)
{
	TRUE
}

#' An S4 class of NMProblem
#'
#' @slot problemStatement The contents of the $PROB statement
#' @slot controlStatements A list of parsed sections of the control file
#' @slot reportStatements A list of parsed sections of the output report file 
#' @slot nmVersionMajor [C,1] NONMEM major version number
#' @slot nmVersionMinor [N,1] NONMEM minor version number
#' @slot inputData A data.frame of the input data, if available (otherwise an empty data.frame) 
#' @slot outputData Aggregation of the output data
#' @slot additionalVars A data.frame of additional variables created by the user
#'
#' @export

setClass("NMProblem", representation("VIRTUAL", 
				problemStatement = "character",
				controlStatements = "list", 
				reportStatements = "list", 
				nmVersionMajor = "character",
				nmVersionMinor = "numeric",
				inputData = "data.frame", outputData = "ANY", additionalVars = "data.frame"),
		validity = validity.NMProblem)


validity.NMBasicModel <- function(object)
{
	
	# Check for list due to the possibility of output tables with FIRSTONLY option
	test1 <- inherits(object@outputData, "data.frame") | inherits(object@outputData, "list") 
	# extract omega estimates.  as.matrix is necessary in case omega matrix is 1x1
	omegaEstimates <- as.matrix(object@omegaFinal[,, "estimates"])
	test2 <- all(diag(omegaEstimates) >= 0)
	test3 <- nrow(object@additionalVars) == 0 || (nrow(object@additionalVars) %% nrow(object@inputData)) == 0
	test4 <- ncol(object@thetaInitial) == ncol(object@thetaFinal)
	
	if(!test1) return("Output data is not a data.frame or list\n")
	if(length(object@objectiveFinal) < 1)
		return("Objective function final value not present!")
	if(length(object@thetaFinal) < 1)
		return("No THETA estimates found!")
	if(all(dim(object@omegaFinal) == 0))
		return("No OMEGA estimates found!")
	
	TRUE
}

#' This class holds the information for a standard single NONMEM problem  
#' 
#' @slot parameterIterations A data.frame of the iteration of each parameter estimate, if available
#' @slot objectiveFinal The numeric value of the objective function minimum
#' @slot thetaInitial Initial theta values
#' @slot thetaFinal Final estimates of the "thetas", together with the standard errors, if available (as a matrix with 1 or 2 rows)
#' @slot sigmaInitial Initial sigma values
#' @slot sigmaFinal Final estimates of the "sigmas", together with the standard errors, if available (as an array  with 1 or 2 matrices)
#' @slot omegaInitial Initial omega values
#' @slot omegaFinal Final estimates of the "omegas", together with the standard errors, if available (as an array  with 1 or 2 matrices)
#' @slot parameterCorMatrix The cross-correlation matrix of the parameter estimators, if available
#' @slot parameterCovMatrix The variance-covariance of the parameter estimators, if available
#' @slot minInfo A string describing the status of the objective function-minimization

setClass(
		"NMBasicModel", 
		representation("NMProblem",
				parameterIterations = "ANY",
				objectiveFinal = "numeric", 
				thetaInitial = "matrix", sigmaInitial = "array", omegaInitial = "array", 
				thetaFinal = "matrix", sigmaFinal = "array",
				omegaFinal = "array", 	 
				parameterCovMatrix = "matrix",
				parameterCorMatrix = "matrix",
				minInfo = "character"),validity = validity.NMBasicModel
)


validity.NMBasicModelNM7 <- function(object)
{
	numMethods <- length( object@methodNames )
	if(! all(numMethods == c( nrow(object@methodInfo), length(object@objectiveFinal), 
					length(object@thetaFinal), length(object@sigmaFinal), 
					length(object@omegaFinal)) ))
		return("Inconsistency between number of methods used and the number of rows of method descriptors, length of theta estimates, sigma estimates, or omega estimates")
	if(! all(sapply(object@ETAShrinkage, function(x) is.numeric(x) | is.null(x) ) ) & all(sapply(object@EPSShrinkage, function(x) is.numeric(x) | is.null(x) ) ) )
		return("Invalid class for shrinkage values")
		
	TRUE
}


setClass(
		"NMBasicModelNM7", 
		representation("NMProblem",
				parameterIterations = "ANY",
				objectiveFinal = "numeric",
				methodInfo = "matrix", methodNames = "character",
				thetaInitial = "matrix", sigmaInitial = "array", omegaInitial = "array", 
				thetaFinal = "list", sigmaFinal = "list",
				omegaFinal = "list",
				thetaStderr = "list", sigmaStderr = "list",
				omegaStderr = "list",
				ETAShrinkage = "list",
				EPSShrinkage = "list",
				parameterCovMatrices = "list",
				parameterCorMatrices = "list",
				minInfo = "character"),
		validity = validity.NMBasicModelNM7
)


# TODO: THETAS might be missing from that particular problem statement, in which case
# they should be inherited from a previous problem.  This is currently not implemented

validity.NMSimDataGen <- function(object)
{
	if(object@numSimulations < 1)
		return("Number of simulations is less than 1!")
	# if(length(object@thetaInitial) < 1)
	#	return("No initial thetas!")
	TRUE
}

#' This class handles NONMEM models that have a $SIM statement but only simulate
#' data without fitting a model
#' 
#' @slot numSimulations Number of simulations/subproblems 
#' @slot seeds Seeds used for the random number generator
#' @slot problemStatement $PROBLEM statement 
#' @slot thetaInitial Fixed/initial theta values used to generate data
#' @slot omegaInitial Fixed/initial omega values used to generate data
#' @slot sigmaInitial Fixed/initial sigma values used to generate data
#'
#' @exportClass NMSimDataGen

setClass(
		"NMSimDataGen", 
		representation(
				"NMProblem", numSimulations = "numeric",	seeds = "numeric", 
				thetaInitial = "vector", omegaInitial = "matrix", sigmaInitial = "matrix"
		),validity = validity.NMSimDataGen
)

validity.NMSimModel <- function(object)
{
	test1 <- object@numSimulations == dim(object@thetaFinals)[1] 
	test1 <- test1 & dim(object@omegaFinal)[3] == dim(object@sigmaFinal)[3] 
	test1 <- test1 & length(object@objectiveFinal)  
	if(!test1)
		return("Incompability between number of simulations and dimension of one of the parameter estimates")
	TRUE
}

#' This class holds the results of NONMEM problems that both simulate data and fit the model during each simulation
#' (that is, models with a $SIM statement in the control file but no "ONLY" keyword).
#' 
#' @slot numSimulations Number of simulations generated
#' @slot objectiveFinal Vector of final values of the objective functions
#' @slot thetaFinal Final estimates of the "thetas", stored as a matrix with one row for each simulation
#' @slot omegaFinal Final estimates of the "omegas", stored as a 3-d array with one matrix for each simulation
#' @slot sigmaFinal Final estimates of the "sigmas", stored as a 3-d array with one matrix for each simulation
#' @slot thetaInitial Initial values of thetas
#' @slot omegaInitial Initial values of omegas
#' @slot sigmaInitial Initial values of sigmas
#' @slot seeds Values of seeds used for random-number generation
#'
#' @exportClass NMSimModel

setClass("NMSimModel", representation("NMProblem", numSimulations = "numeric" ,
				thetaFinal = "matrix", objectiveFinal = "numeric",
				omegaFinal = "array", sigmaFinal = "array", 
				# thetaStdError = "vector", omegaStdError = "matrix",igmaStdError = "matrix", 
				thetaInitial = "vector", 
				omegaInitial = "matrix", sigmaInitial = "matrix", seeds = "numeric"
		))

#' @importFrom methods extends

validity.NMRun <- function(object)
{
	if(length(object@problems) < 1)
		return("Run has no problem results!")
	# check that all problems extend the "NMProblem" class
	
	extendsProbClass <- sapply(object@problems, function(x) extends(class(x), "NMProblem"))
	if(!all(extendsProbClass))
		return("One or more classes in the set of problems do not extend NMProblem")
	
	
	if(object@numProblems != length(object@problems))
		return("Indicated number of problems in object@numProblems is not equal to the length of the problems list!")
	# controlText = "character", 
	# reportText = "character",
	if(length(object@controlText) == 0 || length(object@reportText) == 0)
		return("Empty control file or report file text")
	if(nrow(object@controlFileInfo) == 0 || nrow(object@reportFileInfo) == 0)
		return("Control file information or report file information is empty")
	
	TRUE
}

# declared the "Date" S3 class
setOldClass("Date")

#' This is the basic class for handling a standard NONMEM run.  In essence, it holds
#' several individual problems which may be of type NMBasicModel, NMSimModel, and NMSimDataGen.
#' It also holds the control file and list output file text
#' 
#' @slot controlText [C,+] Text of the control file, without comments
#' @slot controlComments [C,1] Comments of each line of the control file
#' @slot nmVersionMajor [C,1] NONMEM major version number
#' @slot nmVersionMinor [N,1] NONMEM minor version number
#' @slot controlFileInfo [data.frame] Information about the control file 
#' @slot reportFileInfo [data.frame] Information about the list file
#' @slot numProblems [N,1] - Number of problems in the run
#' @slot problems [list] - List of the actual problem results 
#' @slot reportText Text of the lst output file
#' 

setClass(
		"NMRun", 
		representation(
				controlText = "character", 
				reportText = "character",
				nmVersionMajor = "character",
				nmVersionMinor = "numeric",
				controlComments = "character",
				controlFileInfo = "data.frame", 
				reportFileInfo = "data.frame",
				numProblems = "numeric",
				problems = "list"
		),
		validity = validity.NMRun
)



validity.NMSimModelNM7 <- function(object)
{
#	test1 <- object@numSimulations == dim(object@thetaFinals)[1] 
#	test1 <- test1 & dim(object@omegaFinal)[3] == dim(object@sigmaFinal)[3] 
#	test1 <- test1 & length(object@objectiveFinal)  
#	if(!test1)
#		return("Incompatibility between number of simulations and dimension of one of the parameter estimates")
	TRUE
}

#' This class holds the results of NONMEM problems that both simulate data and fit the model during each simulation
#' (that is, models with a $SIM statement in the control file but no "ONLY" keyword).
#' 
#' @slot numSimulations Number of simulations generated
#' @slot objectiveFinal Vector of final values of the objective functions
#' @slot thetaFinal Final estimates of the "thetas", stored as a matrix with one row for each simulation
#' @slot omegaFinal Final estimates of the "omegas", stored as a 3-d array with one matrix for each simulation
#' @slot sigmaFinal Final estimates of the "sigmas", stored as a 3-d array with one matrix for each simulation
#' @slot thetaInitial Initial values of thetas
#' @slot omegaInitial Initial values of omegas
#' @slot sigmaInitial Initial values of sigmas
#' @slot seeds Values of seeds used for random-number generation
#'
#' @slot numMethods TODO
#' @slot methodInfo TODO
#' @slot methodNames TODO
#' 
#' @exportClass NMSimModelNM7

setClass("NMSimModelNM7", representation("NMProblem", numSimulations = "numeric" ,
				thetaFinal = "array", objectiveFinal = "matrix",
				numMethods = "numeric", 
				methodInfo = "matrix", methodNames = "character",
				omegaFinal = "list", sigmaFinal = "list",  
				thetaInitial = "vector", 
				omegaInitial = "matrix", sigmaInitial = "matrix", seeds = "numeric"
		), validity = validity.NMSimModelNM7)

# auxiliary classes for representing report file statements and control file statements

#' @exportClass nmRunReport

setClass("nmRunReport", representation("list"))

#' @exportClass nmModel

setClass("nmModel", representation("list"))
