
# This : PARAMITEMS <- c("final", "initial", "stderrors") is defined in getThetas.R

#' A generic function that extracts omega estimates (and initial estimates and standard errors if specified) from a NONMEM object.
#' For NONMEM 7 objects, shrinkage values will also be available
#' @title Retrieve OMEGA estimates, initial values of 
#' @param obj An object of class NMBasicModel, NMRun, NMSimModel, NMBasicModelNM7, NMSimModelNM7, or nmModel 
#' @param what Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models) 
#' @param subProblemNum [N,+] Numeric vector of simulation sub-problems to use.  Only applies to simulation models
#' @param method [N,+] Vector of methods to extract when dealing with NONMEM 7 problems
#' @param problemNum [N,1] Number of problem to reference - applies to runs only
#' @return For NONMEM 6 - either a list of matrices if multiple 
#' "what=" are used, or a single matrix otherwise For NONMEM 7 - The same as above for a single method.
#' If multiple methods, a list of lists or a list of matrices 
#' @author Mango Solutions
#' @note
#' Invalid \code{what} elements are simply ignored.
#' @examples
#' \dontrun{
#'      x <- importNm("theoph.con", path = "examples/theoph")
#'      getOmegas(x, what = c("initial", "final")) 
#' }

getOmegas <- function(obj, what = "final", subProblemNum = 1, method=length(getMethodNames(obj)), problemNum = 1)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}

setGeneric("getOmegas")

getOmegas.NMBasicModel <- function(obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	omegas <- obj@omegaFinal
	
	# check if the matrices are one-by-one.  If they are, then we will have to coerce to a matrix later on
	
	oneByOne <- all(dim(omegas)[1:2] == c(1,1) )
	finalEstimates <- omegas[,,"estimates", drop = TRUE]
	if(oneByOne) finalEstimates <- matrix(finalEstimates, dimnames = dimnames(omegas)[1:2])
	if("standardErrors" %in% dimnames(omegas)[[3]])
	{
		stdErrors <- omegas[,,"standardErrors", drop = TRUE]
		if(oneByOne) stdErrors <- matrix(stdErrors, dimnames = dimnames(omegas)[1:2])
	}
	else
		stdErrors <- NULL
	initialValues <- obj@omegaInitial
	if(oneByOne) initialValues <- matrix(initialValues, dimnames = dimnames(omegas)[1:2])
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
		# this occurs if the omegas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix	
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

#' @rdname getOmegas
#' @export

setMethod("getOmegas", signature(obj = "NMBasicModel"), getOmegas.NMBasicModel)


.selectMethod <- function(availableMethods, method)
{
	methodChosen <- method[1]
	
	if(class(methodChosen) == "integer") methodChosen <- as.numeric(methodChosen)
	assertClass(methodChosen, "numeric")
	
	if(methodChosen < 1 | methodChosen > length(availableMethods)) RNMImportStop("Invalid method index")
	methodChosen
}

getOmegas.NMBasicModelNM7 <- function(obj, what = "final", subProblemNum = 1, method=length(getMethodNames(obj)), problemNum = 1)
{
	getSigmasOrOmegas.NM7(obj, what = what, item = "omega", method = method )
}

#' @export

setMethod("getOmegas", signature(obj = "NMBasicModelNM7"), getOmegas.NMBasicModelNM7)

getOmegas.NMRun <- function( obj, what = "final", subProblemNum = 1, method=length(getMethodNames(obj)), problemNum = 1 )
{
	dat <- getProblem(obj, problemNum)
	omegas <- getOmegas(dat, what = what, method = method, subProblemNum = subProblemNum)
	omegas
}

#' @rdname getOmegas
#' @export

setMethod("getOmegas", signature(obj = "NMRun"), getOmegas.NMRun)

getOmegas.NMSimModel <- function( obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1 )
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	finalEstimates <- obj@omegaFinal[, , subProblemNum, drop = FALSE]
	initial <- obj@omegaInitial
	
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

#' @rdname getOmegas
#' @export

setMethod("getOmegas", signature(obj = "NMSimModel"), getOmegas.NMSimModel)

getOmegas.NMSimModelNM7 <- function(obj, what = "final", subProblemNum = 1, method=length(getMethodNames(obj)), problemNum = 1)
{
	getSigmasOrOmegas.NM7(obj, what = what, item = "omega", method = method, probType = "sim", subProblemNum = subProblemNum )
}

#' @rdname getOmegas
#' @export

setMethod("getOmegas", signature(obj = "NMSimModelNM7"), getOmegas.NMSimModelNM7)

getOmegas.NMSimDataGen <- function( obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1 )
{
	obj@omegaInitial
}

#' @rdname getOmegas
#' @export

setMethod("getOmegas", signature(obj = "NMSimDataGen"), getOmegas.NMSimDataGen)

getOmegas.nmModel <- function( obj, what = "initial", subProblemNum = 1, method = 1, problemNum = 1 )
{
	# make sure that the problem is not out of bounds
	RNMImportStopifnot(problemNum %in% seq_along(obj$problemContents), "Invalid problem chosen", match.call())
	probResults <- obj$problemContents[[problemNum]]
	
	probResults$Omega
	
}

#' @export

setMethod("getOmegas", signature(obj = "nmModel"), getOmegas.nmModel)
