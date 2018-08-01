
#' purely internal function for reading the various "sections" of the iter file 
#' @param iterSec An element of the list returned by sectionLst  
#' @return Vector
#' @author Mango Solutions

.readIterSection <- function(x)
{
	x <- negGrep( "^[1[:space:]][[:space:]]*$", x, value = TRUE ) # rm lines with only spaces or staring with 1 
	iterNum  <- colonPop( x, "ITERATION[[:space:]]*N?O?\\.?", absent = NA ) # get the iteration number
	objValue <- colonPop( x, "OBJECTIVE VALUE", absent = NA) # get the objective value
	# use the same function for extracting estimates from a list file
	out <- .importNmLstEstimates( x, onlyDiag = TRUE ) # import THETA, diag(OMEGA), diag(SIGMA)
	names(out) <- NULL
	out <- unlist( out )
	return(c(Iteration = as.numeric( iterNum), Objective = as.numeric( objValue), out ))
	
}

#' Imports a NONMEM "INTER" file into a data.frame with one row for each iteration
#' @title Import NONMEM "INTER" file
#' @param fileName [C,1] Name of the "Inter" file to read
#' @param path [C,1] Path that holds the file 
#' @return A data.frame with the iteration information
#' @note Based on code by R. Pugh,J.James, and R.Francois
#' @author Mango Solutions
#' @export

importNmInter <- function( fileName, path = NULL)
{
	
	# read the file
	fileName <- .getFile(fileName, processPath(path))
	txt <- scanFile(fileName)
	logMessage(logName = "detailedReport", paste("Attempting to read ITER file:", fileName, "\n"))
	if(is.null(txt))
	{
		RNMImportWarning(paste("Unable to import the inter file:", fileName, "\n"), call = match.call())
		return(NULL)
	}
	# partition the ITER file using the same function as for list file partitioning, the extract the information
	iter <- lapply( sectionLst( txt, "0" ), .readIterSection)
	as.data.frame( do.call( rbind, iter ) ) 
}

