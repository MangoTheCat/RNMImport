

#' Retrieve the omega or sigma - related items from a NONMEM 7 object.  This is an internal utility routine that was
#' created to refactor getSigmas and getOmegas to avoid code repetition
#' @param obj NONMEM 7 problem 
#' @param what [C,+] Objects to extract.  Equivalent to "what" for getSigmas etc.
#' @param item [C,1] Which item (sigma or omega) to extract
#' @param method [N,+] Vector of methods to extract
#' @title Get sigmas or omegas
#' @return A list or a matrix, depending on whether multiple methods and items are are selected.  
#' @author Mango Solutions

getSigmasOrOmegas.NM7 <- function(obj, what = "final", item = c("sigma", "omega"), method = 1, probType = c("basic", "sim"), subProblemNum = 1)
{
	# choose valid items, discard disallowed objects
	
	validWhat <- intersect(what, c(PARAMITEMS, "shrinkage"))
	invalidWhat <- setdiff(what, c(PARAMITEMS, "shrinkage"))
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	item <- match.arg(item)
	probType <- match.arg(probType)
	
	# function to retrieve objects for a single method
	
	.getSigOrOmSingleMethod <- function(meth = 1)
	{
		# restrict the method chosen to a valid one
		
		methodChosen <- .selectMethod(obj@methodNames, meth)
		#capture the report file name of the method
		
		methodName <- obj@methodNames[methodChosen]
		
		
		if(item == "sigma")
		{
			finals <- obj@sigmaFinal
			initials <- obj@sigmaInitial
			stdErrs <- obj@sigmaStderr
			shrinks <- obj@EPSShrinkage
		}
		else
		{
			finals <- obj@omegaFinal
			initials <- obj@omegaInitial
			stdErrs <- obj@omegaStderr	
			shrinks <- obj@ETAShrinkage
		}
		# choose single final value
		final <- finals[[methodChosen]]
		# initialize to avoid missing object error
		oneByOne <- FALSE
		if(!is.null(final)) {
			# check if the final estimate matrix is 1-by-1.  If so, force it to remain as a matrix
			# even when drop = FALSE tries to make it otherwise
			oneByOne <- all(dim(final)[1:2] == c(1,1) )
			finalEstimates <- final
			if(oneByOne) finalEstimates <- matrix(finalEstimates, dimnames = dimnames(final)[1:2])
		}
		else
			finalEstimates <- NULL
		if(!is.null(stdErrs[[methodChosen]]))
		{
			stdErrors <- stdErrs[[methodChosen]]
			if(oneByOne) stdErrors <- matrix(stdErrors, dimnames = dimnames(final)[1:2])
		}
		else stdErrors <- NULL
		shrinkage <- shrinks[[methodChosen]]
		
		# initial value depends on the number of the method 
		
		if(methodChosen == 1)
			initialValues <- initials
		else initialValues <- finals[[methodChosen - 1]]
		
		if( length(final) > 0 && oneByOne ) initialValues <- matrix(initialValues, dimnames = dimnames(final)[1:2])
		
		# no valid option selected, thrown an error
		if(length(validWhat) == 0) RNMImportStop("No valid items selected for retrieval!", call = match.call())
		
		# single object selected, don't return a list
		
		if(length(validWhat) == 1)
		{
			res <- switch(validWhat, 
					"final" = finalEstimates,
					# TODO: if these are length 0, generate an error?
					"initial" = initialValues,
					"stderrors" = 	stdErrors,
					"shrinkage" = shrinkage
			)
			if(length(res) == 0)
				RNMImportWarning( paste(validWhat, "is not available, returning NULL") )
		} # end if length(validWhat) == 1
		
		# return a list of the selected objects
		
		else
		{
			res <- list()

			if("initial" %in% validWhat) res$initial.estimates <- initialValues
			if("final" %in% validWhat) res$final.estimates <- finalEstimates
			if("stderrors" %in% validWhat) 
			{
				if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
				else res$standard.errors <- stdErrors
			}
			if("shrinkage" %in% validWhat)	res$eps.shrinkage <- shrinkage
		}
		if(!is.null(res)) attr(res, "methodName") <- methodName
		res
	}
	
	.getSigOrOmSingleMethodSim <- function(meth = 1)
	{
		# select the method
		methodChosen <- .selectMethod(obj@methodNames, meth)
		methodName <- obj@methodNames[methodChosen]
		
		if(item == "omega")
		{
			finals <- obj@omegaFinal
			initial <- obj@omegaInitial
		}
		else
		{
			finals <- obj@sigmaFinal
			initial <- obj@sigmaInitial
		}
		
		final <- finals[[methodChosen]]
		
		if("stderrors" %in% validWhat)
			RNMImportWarning(msg = "No standard errors are available!")
		
		numSimulations <- obj@numSimulations
		if(any(!(subProblemNum %in% 1:numSimulations)))
			RNMImportStop(msg = "Subproblem number is not valid!")	
		
		if(!is.null(final))
			finalEstimates <- final[, , subProblemNum, drop = FALSE]
		else finalEstimates <- NULL
		
		if(methodChosen == 1)
			initialValues <- initial
		else initialValues <- finals[[methodChosen - 1]][,,subProblemNum, drop = FALSE]
		
		if(length(validWhat) == 1)
		{
			res <- switch(validWhat, 
					"final" = finalEstimates,
					"initial" = initialValues
			)
			if(length(res) == 0)
				RNMImportWarning( paste(validWhat, "is not available, returning NULL") )
			# this occurs if the omegas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix	
		} # end if length(validWhat) == 1
		else
			res <- list("initial.estimates" = initialValues, "final.estimates"  = finalEstimates)
		if(!is.null(res)) attr(res, "methodName") <- methodName
		res
	}
	
	retrievalFunc <- if(probType == "sim") .getSigOrOmSingleMethodSim else .getSigOrOmSingleMethod
	
	if(length(method) > 1) 
	{
		# if more than one method selected, extract all of them
		x <- lapply(method, retrievalFunc)
		# names(x) <- paste("METHOD", seq_along(obj@methodNames), sep = "")
		x
	}
	else
		retrievalFunc(method)
}