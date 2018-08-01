

PARAMITEMS <- c("final", "initial", "stderrors")


#' A generic function that extracts theta estimates (and initial estimates and standard errors if specified) from a NONMEM object.
#' @param obj An object of class NMBasicModel, NMRun, NMSimModel, NMBasicModelNM7, NMSimModelNM7, or  nmModel
#' @param what Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models) 
#' @param subProblemNum Numeric vector of simulation sub-problems to use.  Only applies to simulation models
#' @param method Vector of methods to extract when dealing with NONMEM 7 problems
#' @param problemNum Number of problem to use - applies to NMRun only
#' @return A matrix of named rows for final estimates, initial estimates, standard errors etc. as applicable, or a list
#' of matrices if multiple methods are chosen in NONMEM 7
#' @author Mango Solutions
#' @note Invalid \code{what} elements are simply ignored.
#' @keywords methods
#' @examples
#' \dontrun{
#'      x <- importNm("theoph.con", path = "examples/theoph")
#'      getThetas(x, what = c("initial", "final")) 
#' }

getThetas <- function(obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1 )
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getThetas")

getThetas.NMBasicModel <- function( obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	thetas <- obj@thetaFinal
	numRow <- nrow(thetas)
	
	finalEstimates <- thetas["estimates",, drop = TRUE]

	if("standardErrors" %in% dimnames(thetas)[[1]])
		stdErrors <- thetas["standardErrors",, drop = TRUE]
	else stdErrors <- NULL
	initialValues <- obj@thetaInitial
	
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
		res <- matrix(ncol = ncol(thetas), nrow = 0, dimnames = list(NULL, colnames(thetas)))

		if(prod(dim(initialValues ))>0){
			if("initial" %in% validWhat) res <- rbind(res, initialValues[,1:dim(res)[2], drop=FALSE] )
		}
		if("final" %in% validWhat) res <- rbind(res, "estimates" = finalEstimates)
		if( "stderrors" %in% validWhat )
		{
			if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
			else res <- rbind(res, "standardErrors" = stdErrors)
		}
	}
	res
	
}

setMethod("getThetas", signature(obj = "NMBasicModel"), getThetas.NMBasicModel)

getThetas.NMRun <- function( obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1 )
{
	dat <- getProblem(obj, problemNum)
	thetas <- getThetas(dat, what = what, method = method, subProblemNum = subProblemNum)
	thetas	
}
setMethod("getThetas", signature(obj = "NMRun"), getThetas.NMRun)

getThetas.NMBasicModelNM7 <- function( obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1 )
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	.getThetasSingleMethod <- function (meth = 1) 
	{
		methodChosen <- .selectMethod(obj@methodNames, meth)
		thetas <- obj@thetaFinal[[methodChosen]]
		if(is.null(thetas)){
			RNMImportWarning(paste('THETAS not available for method', meth))
			return(NULL)
		}
		finalEstimates <- thetas 
		stdErrors <- obj@thetaStderr[[methodChosen]]
		# the initial values depend on the method chosen
		if(methodChosen == 1)
			initialValues <- obj@thetaInitial
		else
		{
			# this has the estimates only, not the upper and lower bounds
			x <- obj@thetaFinal[[methodChosen-1]]
			# extract this to have access to the upper and lower bounds
			initialValues <- obj@thetaInitial
			if('Prior' %in% names(obj@controlStatements)){
				initialValues["initial", 1:obj@controlStatements$Prior['nTheta']] <- unname(x)
			} else {
				initialValues["initial", ] <- unname(x)
			}
		}
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
			any(regexpr('PRIOR=*', obj@controlStatements$Sub)>0)
			if('Prior' %in% names(obj@controlStatements)){
                res <- matrix(ncol = length(thetas), nrow = 0, 
                        dimnames = list(NULL, dimnames(initialValues)[[2]][1:obj@controlStatements$Prior['nTheta']]))
			} else {
				res <- matrix(ncol = length(thetas), nrow = 0, 
						dimnames = list(NULL, dimnames(initialValues)[[2]]))
			}
			if(prod(dim(initialValues ))>0){
				if("initial" %in% validWhat) {
					if('Prior' %in% names(obj@controlStatements)){
						res <- 
								rbind(res, initialValues[1:obj@controlStatements$Prior['nTheta']] )
					} else {
						res <- rbind(res, initialValues )
					}
				}
			}
			
			if("final" %in% validWhat) res <- rbind(res, "estimates" = finalEstimates)
			if( "stderrors" %in% validWhat )
			{
				if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
				else res <- rbind(res, "standardErrors" = stdErrors)
			}
		}
		attr(res, "methodName") <- obj@methodNames[[methodChosen]]
		res
	}
	if(length(method) > 1)
		lapply(method, .getThetasSingleMethod)
	else
		.getThetasSingleMethod(method)
}

setMethod("getThetas", signature(obj = "NMBasicModelNM7"), getThetas.NMBasicModelNM7)

getThetas.NMSimModel <- function( obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1 )
{
	
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	
	finalEstimates <- obj@thetaFinal[subProblemNum,]
	
	initial <- obj@thetaInitial
	
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
setMethod("getThetas", signature(obj = "NMSimModel"), getThetas.NMSimModel)


getThetas.NMSimModelNM7 <- function(obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	
	.getThetasSingleMethod <- function(meth = 1) {
		
		methodChosen <- .selectMethod(obj@methodNames, meth)
		finalEstimates <- obj@thetaFinal[subProblemNum,,methodChosen]
		# the initial values depend on the method chosen
		if(methodChosen == 1)
			initial <- obj@thetaInitial
		else
			# this has the estimates only, not the upper and lower bounds
			initial <- obj@thetaFinal[subProblemNum,,methodChosen - 1]
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
		# attach the name of the method used
		attr(res, "methodName") <- obj@methodNames[[methodChosen]]
		res
	}
	
	if(length(method) > 1)
		lapply(method, .getThetasSingleMethod)
	else
		.getThetasSingleMethod(method)
}
setMethod("getThetas", signature(obj = "NMSimModelNM7"), getThetas.NMSimModelNM7)

getThetas.NMSimDataGen <- function(obj, what = "initial", subProblemNum = 1, method = 1, problemNum = 1 )
{
	obj@thetaInitial
}

setMethod("getThetas", signature(obj = "NMSimDataGen"), getThetas.NMSimDataGen)

getThetas.nmModel <- function( obj, what = "initial", subProblemNum = 1, method = 1, problemNum = 1 )
{
	# make sure that the problem is not out of bounds
	RNMImportStopifnot(problemNum %in% seq_along(obj$problemContents), "Invalid problem chosen", match.call())
	probResults <- obj$problemContents[[problemNum]]
	# transpose to be in a more standard format
	t(probResults$Theta)
	
}

setMethod("getThetas", signature(obj = "nmModel"), getThetas.nmModel)
