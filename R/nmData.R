
#' A generic function that extracts input and output data tables from a NONMEM object into either a single consolidated
#' data.frame or a list.  
#' @param obj An object of class NMRun, or one that inherits from NMProblem
#' @param dataTypes Which type of data should be returned, must 
#' be "input" and/or "output". These correspond to data for the object which correspond to either inputs (as specified in the $DATA statement)
#' or outputs (specified by the $TABLE statement)
#' @param returnMode Determines how the data should be returned.  If "singleDF", attempts to return a consolidated data.frame of output
#' and input data, if "DFList" returns a list with seperate input and output data
#' @param ... Additional parameters: problemNum to select the problem if obj is of class NMRun, subProblemNum to select a 
#' set of subproblems for simulation problems
#' @param subset Controls the application of a "subset" to the data when it is returned. In general this is used to remove dosing records from the data. If NULL or FALSE, nothing is done. 
#' Otherwise, it should be a character vector of subsetting expressions (such as "MDV = 1", etc.) which will be applied in succession, OR
#' it may be \code{TRUE}, in which case the associated data subset of the object (retrieved by \code{\link{dataSubset}}) will be applied.
#' @title Extract input and / or output data tables from a NONMEM object
#' @return A data.frame or a list of data.frames of data, depending on the value of 
#' \code{returnMode}. If a data.frame is requested and If variables are found in both input 
#' and output tables, then the variable from the output will appear in the result data frame
#'  without modification. The input data will then appear with the postfix \code{.INPUT} added
#'  to the column names. If any of the output tables generated were produced with a FIRSTONLY 
#' flag, these will be discarded by nmData (this should change in a future version of the 
#' package). 
#' @author Mango Solutions
#' @examples
#' \dontrun{
#'  x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "testing/testdata/TestRun")
#'  x.data <- nmData(x)
#'  print(head(x.data))
#'  x.data <- nmData(x, returnMode = "DFList" )
#'  print(lapply(x.data, head))
#'  y <- importNm("TestData1SIM.con", "TestData1SIM.lst", path = "testing/testdata/TestSimRun")
#'  y.outputData <- nmData(y, subProblemNum = 2:4, dataTypes = "output")
#'  print(summary(y.outputData))
#' }
#' @keywords utilities datagen
#' 
 

# TODO: add option to extract from derived data

nmData <- function(obj, dataTypes = c("input", "output") , returnMode = c("singleDF", "DFList"), 
		subset = NULL, ...)
{
	returnMode <- match.arg(returnMode)
	RNMImportStop("This method is not implemented for this class\n")
}

setGeneric("nmData")

#' utility function for determining what subset should be applied to a dataset. 
#' @param obj 
#' @param subset The subset parameter passed to nmData - a logical, NULL, or a character vector
#' @return NULL or a character vector
#' @author Mango Solutions

.getSubset <- function(obj, subset)
{
	if( class(subset) == "logical" ) {
		if(subset[1])
			return(dataSubset( obj ))
		else return(NULL)
	}
	# if not logical, dataSub is just equal to subset
	else
		subset
}

nmData.NMBasicModel <- function(obj, dataTypes = c("input", "output") , returnMode = c("singleDF", "DFList"),
		subset = NULL, ...)
{

	dataTypes <- intersect(dataTypes, c("input", "output"))
	
	if(length(dataTypes) == 0)
		RNMImportStop("No valid datatypes selected for retrieval\n", match.call())
	
	returnMode <- match.arg(returnMode)
	
	# check for FIRSTONLY
	if(class(obj@outputData) == "list") {
		if("output" %in% dataTypes) 
			RNMImportWarning("FIRSTONLY output data currently ignored\n")
		outputData <- obj@outputData[["normal.tables"]]
		
	}
	else outputData <- obj@outputData
	# if subset is supplied, handle it and store the result in dataSub
	# check that it is logical, and obtain an appropriate subset if it is
	
	dataSub <- .getSubset(obj, subset)
	
	allData = list("input" = obj@inputData, "output" = outputData)
	
	# only one data.frame to return
	
	if(length(dataTypes) == 1)
		return(applyDataSubset(allData[[dataTypes]], dataSub))
	
	# more than one data type
	if(returnMode == "DFList")
		return(lapply(allData[dataTypes], function(x) applyDataSubset(x, dataSub)))
	
	outData <- allData$output
	inData <- allData$input
	inColumns <- colnames(inData)
	outColumns <- colnames(outData)
	
	# otherwise, bind the data together, taking care to deal with repeated data.
	allColumns <- union(inColumns, outColumns)
	clashingColumns <- intersect(inColumns, outColumns)
	
	# no repeated columns, so just return cbind
	
	if(length(clashingColumns) == 0)
	{
		return(applyDataSubset(cbind(inData, outData), dataSub))
	}
	
	# create names of the form "VAR.INPUT", "VAR.OUPUT" etc. for those columns found in both data sets.
	# Note that a variable name with 
	
	# determine the names unique to both input and output data
	
	uniqueIn <- setdiff(inColumns, clashingColumns)
	uniqueOut <- setdiff(outColumns, clashingColumns)
	
	res <- cbind(outData, inData[uniqueIn])
	clashIn <- inData[,clashingColumns, drop = FALSE]
	
	names(clashIn) <- paste(clashingColumns, "INPUT", sep = ".")
	applyDataSubset( cbind(res, clashIn), dataSub )
}

setMethod("nmData", signature(obj = "NMBasicModel"), nmData.NMBasicModel)
setMethod("nmData", signature(obj = "NMBasicModelNM7"), nmData.NMBasicModel)

nmData.NMSim<- function(obj, dataTypes = c("input", "output") , 
		returnMode = c("singleDF", "DFList"),  
		subset = NULL, subProblemNum = NA, stackInput = TRUE)
{

	# if subset is supplied, handle it and store the result in dataSub
	# check that it is logical, and obtain an appropriate subset if it is
	
	returnMode <- match.arg(returnMode)
	dataSub <- .getSubset(obj, subset)
	
	dataTypes <- intersect(dataTypes, c("input", "output"))
	
	if(length(dataTypes) == 0)
		RNMImportStop("No valid datatypes selected for retrieval\n", match.call())
	
	inData <- obj@inputData
	returnMode <- match.arg(returnMode)

	if(class(obj@outputData) == "list") {
		if("output" %in% dataTypes ) 
			RNMImportWarning("FIRSTONLY output data currently ignored\n")
		outData <- obj@outputData[["normal.tables"]]
		
	}
	else outData <- obj@outputData
	if(is.na(subProblemNum)) subProblemNum = 1:obj@numSimulations
	
	if("output" %in% dataTypes)
	{
	
		# create a simulation number factor
		simNum <- gl(obj@numSimulations, nrow(outData) / obj@numSimulations , ordered = TRUE)
		outData <- cbind(outData, "NSIM" = simNum)
		# extract requested simulations
		if(all(is.na(subProblemNum))) subProblemNum = 1:obj@numSimulations
		outData <- subset(outData, NSIM %in% subProblemNum)
	}
	# only one data.frame to return
	if(length(dataTypes) == 1)
	{
		res <- if(dataTypes == "input") inData else outData
		res <- applyDataSubset(res, dataSub)
		return(res)
	}
	# more than one data type
	if(returnMode == "DFList"){
		res <- list("input" = inData, "output" = outData)
		# take the subsets as needed
		res <- lapply(res, function(x) applyDataSubset(x, sub = dataSub))
		return(res)
	}
	# if stackInput == TRUE, replicate the input data set so that its number of rows matches
	# the number of rows of the simulated output data set

	if(stackInput)
		inData <- do.call(cbind.data.frame, lapply(inData, base::rep, length(subProblemNum)))
	if(nrow(inData) != nrow(outData))
		RNMImportStop("Amount of simulated output data selected is not compatible with the amount of input data, cannot bind into a single data.frame\n",
				call = match.call())
	
	inColumns <- colnames(inData)
	outColumns <- colnames(outData)
	# otherwise, bind the data together, taking care to deal with repeated data.
	allColumns <- union(inColumns, outColumns)
	clashingColumns <- intersect(inColumns, outColumns)
	# no repeated columns, so just return cbind
	if(length(clashingColumns) == 0)
	{
		return(applyDataSubset(cbind(inData, outData), dataSub))
	}
	# create names of the form "VAR.INPUT", "VAR.OUPUT" etc. for those columns found in both data sets.
	# Note that a variable name with 
	
	# determine the names unique to both input and output data
	uniqueIn <- setdiff(inColumns, clashingColumns)
	uniqueOut <- setdiff(outColumns, clashingColumns)
	res <- cbind(outData, inData[uniqueIn])
	clashIn <- inData[,clashingColumns, drop = FALSE]
	
	names(clashIn) <- paste(clashingColumns, "INPUT", sep = ".")
	
	applyDataSubset(cbind(res, clashIn), dataSub)
	
}

setMethod("nmData", signature(obj = "NMSimDataGen"), nmData.NMSim)
setMethod("nmData", signature(obj = "NMSimModel"), nmData.NMSim)
setMethod("nmData", signature(obj = "NMSimModelNM7"), nmData.NMSim)

nmData.NMRun <- function(obj, dataTypes = c("input", "output") , returnMode = c("singleDF", "DFList"),
		subset = NULL, problemNum = 1, subProblemNum = NA)
{
	returnMode <- match.arg(returnMode)
	nmData(getProblem(obj, problemNum),dataTypes,returnMode, subset = subset, subProblemNum)
}

setMethod("nmData", signature(obj = "NMRun"), nmData.NMRun)
