
#' Extracts the final estimates of the thetas, omegas, and sigmas from the text of a report file
#' @title import report file estimates
#' @param txt Contents of a report file, as a vector of lines of text 
#' @param onlyDiag Should only the diagonal of the omegas and sigmas be retrieved?  
#' @return a list with elements "THETA", "OMEGA" and "SIGMA", holding vectors and matrices
#' 
#' @author Mango Solutions

.importNmLstEstimates <- function( txt, onlyDiag = FALSE  )
{
	# imports sections : FINAL PARAMETER ESTIMATE, STANDARD ERROR OF ESTIMATE
	
	if( is.null(txt)) return (NULL)
	
    # remove possible data time
	txt <- gsub('[A-Z][a-z]{2} +[A-Z][a-z]{2} +\\d+ +\\d+:\\d+:\\d+ +[A-Z]{3} +\\d+','',txt)
	### titles of subsections look like:
	#   THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********
	#   OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********
	#   SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ***
	sections <- lseq( c( grep( "[[:alpha:]]+[[:space:]]*-[[:space:]][[:alpha:]]+", txt), length(txt)+1 ), txt )  
	names(sections) <- sub( "[[:space:]]*([[:alpha:]]*)[[:space:]]*-.*$", "\\1", sapply( sections, "[", 1) ) # THETA, OMEGA, SIGMA
	sections <- lapply( sections, function(x) x[-1] )                          # remove the first lines
	
	### make R objects from text
	sections[["THETA"]] <-  .importNmLstVector( sections[["THETA"]] ) 
	sections[["OMEGA"]] <-  .importNmLstMatrix( sections[["OMEGA"]], onlyDiag = onlyDiag ) 
	sections[["SIGMA"]] <-  .importNmLstMatrix( sections[["SIGMA"]], onlyDiag = onlyDiag )
	sections
}

