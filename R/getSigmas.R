

#' A generic function that extracts sigma estimates (and initial estimates and standard errors if specified) from a NONMEM object.
#' Shrinkage values may also be retrieved for NONMEM 7 objects.
#' @param obj An object of class NMBasicModel, NMRun, NMSimModel, NMBasicModelNM7, NMSimModelNM7, or nmModel 
#' @param what Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models) 
#' @param subProblemNum Numeric vector of simulation sub-problems to use.  Only applies to simulation models
#' @param method Numeric vector of methods to extract when dealing with NONMEM 7 problems
#' @param problemNum Number of problem to reference - applies to runs only
#' @return For NONMEM 6 - either a list of matrices if multiple "what=" are used, or a single matrix otherwise
#' For NONMEM 7 - The same as above for a single method.  If multiple methods, a list of lists or a list of matrices
#' @author Mango Solutions
#' @note Invalid \code{what} elements are simply ignored.
#' @keywords methods
#' @examples
#' \dontrun{
#'      x <- importNm("theoph.con", path = "examples/theoph")
#'      getSigmas(x, what = c("initial", "final")) 
#' }

getSigmas <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getSigmas")

getSigmas.NMBasicModel <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	sigmas <- obj@sigmaFinal
	oneByOne <- all(dim(sigmas)[1:2] == c(1,1) )
	finalEstimates <- sigmas[,,"estimates", drop = TRUE]
	if(oneByOne) finalEstimates <- matrix(finalEstimates, dimnames = dimnames(sigmas)[1:2])
	if("standardErrors" %in% dimnames(sigmas)[[3]]) 
	{
		stdErrors <- sigmas[,,"standardErrors", drop = TRUE]
		if(oneByOne) stdErrors <- matrix(stdErrors, dimnames = dimnames(sigmas)[1:2])
	}
	else
		stdErrors <- NULL
	initialValues <- obj@sigmaInitial
	if(oneByOne) initialValues <- matrix(initialValues, dimnames = dimnames(sigmas)[1:2])
	
	# no valid option selected, thrown an error
	if(length(validWhat) == 0) RNMImportStop("No valid items selected for retrieval!", call = match.call())
	if(length(validWhat) == 1)
	{
		res <- switch(validWhat, 
				"final" = finalEstimates,
				# TODO: if these are length 0, generate an error?
				"initial" = initialValues,
				"stderrors" = {
					if(is.null(stdErrors))
						RNMImportStop("Standard errors not available \n", call = match.call())
					stdErrors
				}
		)
		# this occurs if the sigmas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix
		if(is.null(dim(res))) 
		{ 
			dim(res) <- c(1,1)
			dimnames(res) <- dimnames(sigmas)[1:2]	
		}	
	} # end if length(validWhat) == 1
	else
	{
		res <- list()
		# TODO: check for missing initial values?
		if("initial" %in% validWhat) res$initial.estimates <- initialValues
		if("final" %in% validWhat) res$final.estimates <- finalEstimates
		if("stderrors" %in% validWhat) 
		{
			if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
			else res$standard.errors <- stdErrors
		}
	}
	res
}

setMethod("getSigmas", signature(obj = "NMBasicModel"), getSigmas.NMBasicModel)

getSigmas.NMBasicModelNM7 <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	getSigmasOrOmegas.NM7(obj, what = what, item = "sigma", method = method)
}


setMethod("getSigmas", signature(obj = "NMBasicModelNM7"), getSigmas.NMBasicModelNM7)

getSigmas.NMRun <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	dat <- getProblem(obj, problemNum)
	sigmas <- getSigmas(dat, what = what, method = method, subProblemNum = subProblemNum)
	sigmas
}
setMethod("getSigmas", signature(obj = "NMRun"), getSigmas.NMRun)

getSigmas.NMSimModel <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	finalEstimates <- obj@sigmaFinal[, , subProblemNum, drop = FALSE]
	
	initial <- obj@sigmaInitial
	
	if(length(validWhat) == 1)
	{
		res <- switch(validWhat, 
				"final" = finalEstimates,
				# TODO: if these are length 0, generate an error?
				"initial" = initial
		)
		# this occurs if the omegas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix	
	} # end if length(validWhat) == 1
	else
		res <- list("initial.estimates" = initial, "final.estimates"  = finalEstimates)
	
	res
}

setMethod("getSigmas", signature(obj = "NMSimModel"), getSigmas.NMSimModel)

getSigmas.NMSimModelNM7 <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	getSigmasOrOmegas.NM7(obj, what = what, item = "sigma", method = method, probType = "sim", subProblemNum = subProblemNum )
}

setMethod("getSigmas", signature(obj = "NMSimModelNM7"), getSigmas.NMSimModelNM7)

getSigmas.NMSimDataGen <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	obj@sigmaInitial
}

setMethod("getSigmas", signature(obj = "NMSimDataGen"), getSigmas.NMSimDataGen)

getSigmas.nmModel <- function(obj, what = "final",  subProblemNum = 1, method = 1, problemNum = 1)
{
	# make sure that the problem is not out of bounds
	RNMImportStopifnot(problemNum %in% seq_along(obj$problemContents), "Invalid problem chosen", match.call())
	probResults <- obj$problemContents[[problemNum]]
	
	probResults$Sigma
	
}

setMethod("getSigmas", signature(obj = "nmModel"), getSigmas.nmModel)