

#' Imports the list file parameter iteration information as a data.frame
#' @param iterList {LIST] A list of iteration text chunks, typically obtained from calling sectionLst 
#' @return A 3 column data.frame with the iterations for each parameter estimate
#' @note Based on code by R. Francois, R. Pugh and J. James
#' @author Mango Solutions
.importNmLstIter <- function(iterList)
{
	
	if( is.null(iterList) ) return( NULL )
	# determine which elements of iterList contain information about iterations, rather than
	iters <- sapply( iterList, function(x){
				grep.out <- grep( "^ITERATION NO\\.", x )
				if( !length(grep.out) ) grep.out <- 0              
				grep.out
			})
		

#   extract the elements of the list which contain minimization information, rather than iteration information
	minInfoPos <- which(sapply(iterList, function(x) any(regexMatches(x, rx = "MINIMIZATION"))))
	minInfo <- unlist(iterList[minInfoPos])
	# if the information was not missing, extract other elements

	if(length(minInfo)>0)
	{
#		if there are warning messages about the minimization then we must condense to one string
		keep.minInfo <- minInfo
		minResult <- equalExpressionPop(  minInfo, "MINIMIZATION", sep = "[[:space:]]*", inPlace=TRUE     )
		if(length(minResult)>1){
			minResult <- paste(keep.minInfo[grep("MINIMIZATION", keep.minInfo)], collapse='\n')
		}
		numEval   <- colonPop( minInfo, "NO\\. OF FUNCTION EVALUATIONS USED", inPlace = TRUE     )
		numSigDigits <- colonPop( minInfo, "NO\\. OF SIG\\. DIGITS IN FINAL EST\\.", inPlace = TRUE )
	}
	
	iterInfo <- iterList[ as.logical(iters) ]
	
	# Internal function for breaking down the information for each iteration
	
	.extractIterInfo <- function(x)
	{
		# extract ITERATION NO.: ..., OBJECTIVE VALUE:  etc.
		iters  <- colonPop( x, "ITERATION NO\\."               , numeric = TRUE, inPlace = TRUE)
		obj    <- colonPop( x, "OBJECTIVE VALUE"               , numeric = TRUE, inPlace = TRUE)
		cumev  <- colonPop( x, "CUMULATIVE NO. OF FUNC. EVALS.", numeric = TRUE, inPlace = TRUE)
		evals  <- colonPop( x, "NO\\. OF FUNC\\. EVALS\\."     , numeric = TRUE, inPlace = TRUE)    
		
		x <- stripBlanks( x , remove.empty = TRUE)
		parStart <- grep( "^PARAMETER:", x )
		graStart <- grep( "^GRADIENT:"  ,x )
		# obtain number of parameter values
		size <- graStart - parStart
		x <- gsub( "^.*:", "", x )
		pars <- as.numeric( .readValues( x[ seq( from = parStart, length.out = size ) ] ) )
		grad <- as.numeric( .readValues( x[ seq( from = graStart, length.out = size ) ] ) )
		
		data.frame(paramNum = seq(along = pars) , iterationNum = iters, 
				objective = obj, numFuncEvals = evals, 
				parameterVal = pars,  gradient = grad)		
	} # end .extractIterInmfo
	
	out <- lapply( iterInfo, .extractIterInfo)
	out <- do.call( rbind, out )
	if(length(minInfo)>0)
		attr( out, "min.info") <- list( minResult = minResult, numEval = numEval, 
				numSigDigits = numSigDigits )
	out
	
}

