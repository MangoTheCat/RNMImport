


#' This generic routine extracts the parameter estimate iteration information of a NONMEM problem, 
#' as produced within the report file or NONMEM 7 iteration file as applicable.  
#' @title Extract parameter estimate iteration information from a given NONMEM problem
#' @aliases getIterations,NMRun-method getIterations,NMBasicModel-method getIterations,NMBasicModelNM7-method
#' @param obj Object of class NMRun, NMBasicModel, or NMBasicModelNM7
#' @param problemNum Number of the problem of NONMEM object 
#' @return If an object produced by NONMEM <= 6 was selected, a data.frame of parsed information from the 
#' report file.  For NONMEM 7, a list generated from the iteration files. The contents of each file are imported
#' and then aggregated into a single list split by estimation method.
#' @author Mango Solutions
#' @keywords utilities
#' @export

getIterations <- function(obj, problemNum = 1)
{
	RNMImportStop("Not defined for objects of this class")
}

setGeneric("getIterations")

getIterations.NMRun <- function(obj, problemNum = 1)
{
	return(getIterations(getProblem(obj, problemNumber = problemNum)))
}

setMethod("getIterations", signature(obj = "NMRun"), getIterations.NMRun)

getIterations.NMProb <- function(obj, problemNum = 1)
{
	obj@parameterIterations
}

setMethod("getIterations", signature(obj = "NMBasicModel"), getIterations.NMProb)
setMethod("getIterations", signature(obj = "NMBasicModelNM7"), getIterations.NMProb)
