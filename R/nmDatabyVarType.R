

#' Retrieves data stored in a NONMEM object by category of data type, e.g. covariates, 
#' "etas", etc.  The type mappings are defined in the configuration data, and may be changed with
#' get/setNmVarDescription
#' @title Get NONMEM data by variable type
#' @param obj Object of class NMRun or NMProblem
#' @param varTypes Character vector of types to return.  Allowed types are currently "Parameter", 
#' "Lab covariate", "Covariate" and "Eta"
#' @param returnMode A string, either "singleDF" or "DFList".  If "singleDF", all of the data
#' is returned in a consolidated data.frame, otherwise as a list returned by type
#' @param ... Additional parameters: problemNum to select the problem if obj is of class NMRun, subProblemNum to select a 
#' set of subproblems for simulation problems
#' @return A data.frame, or list, with the data
#' @author Mango Solutions
#' @seealso \code{\link{nmData}}, \code{\link{getVarDescription}} 
#' @keywords datagen utils
#' @examples
#' \dontrun{
#'      x <- importNm("TestData1.ctl", "TestData1.lst", path  = "testing/testdata/TestRun"))
#'      x.covariates <- nmDatabyVarType(x, varTypes = "Covariate")
#'      print(head(x.covariates))
#' }

nmDatabyVarType <- function(obj, varTypes, returnMode = c("singleDF", "DFList"), ... )
{
	RNMImportStop("nmDatabyVarType not implemented for this class. \n", match.call())
}

setGeneric("nmDatabyVarType")

nmDatabyVarType.NMRun <- function(obj, varTypes, returnMode = c("singleDF", "DFList"),
		problemNum=1,subProblemNum=NA)
{
	nmDatabyVarType(getProblem(obj, problemNum), varTypes, returnMode, subProblemNum)
}

setMethod("nmDatabyVarType", signature(obj="NMRun") ,nmDatabyVarType.NMRun)

nmDatabyVarType.NMProblem <- function(obj, varTypes, returnMode = c("singleDF", "DFList"), subProblemNum=NA)
{
	varTypes <- CSLtoVector(varTypes)
	returnMode <- match.arg(returnMode)
	allowedVarTypes <- c("Parameter", "Lab covariate", "Covariate", "Eta")
	disallowed <- setdiff( varTypes, allowedVarTypes)
	# check to make sure that the user did not specify some unallowed type
	if(length(disallowed) > 0)
	{
		RNMImportStop("The following variable types are not allowed: " %pst% paste(disallowed, collapse = ","),
		 match.call())
	}
	allData <- nmData(obj, returnMode = "singleDF", subProblemNum=subProblemNum)
	# remove all clashing columns
	# TODO: add this option to nmData

	omit <- grep(colnames(allData), pattern = "\\.INPUT")
	allData <- allData[,-omit]
	descriptions <- getVarDescription(colnames(allData))
	# select those columns with descriptions only
	subData <- allData[,descriptions[,1]]
	# perform matching
	dataByType <- lapply(varTypes, function(x) subData[descriptions[,4] == x])

	# now grab ETAs
	etas <- allData[,grep(colnames(allData), pattern = "^ETA[0-9]+")]
	dataByType <- c(dataByType, list("Eta" = etas))
	emptySets <- sapply(dataByType, is.null)
	
	if(returnMode == "singleDF")
		do.call(cbind, dataByType[!emptySets])
	else
	{	
		result <- dataByType[!emptySets]
		names(result) <- varTypes[!emptySets]
		return(result)
		
	}
}

setMethod("nmDatabyVarType", signature(obj="NMProblem") ,nmDatabyVarType.NMProblem)