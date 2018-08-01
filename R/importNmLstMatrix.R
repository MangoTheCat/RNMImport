#' Obtains a matrix from a chunk of report file text (such as the estimates of sigma or omega).  
#' It is meant to be used on elements of the list returned by sectionLst
#' @title Extract matrix from a report file
#' @param txt [C,+] character vector of lines of text
#' @param condensed [L,1] Logical flag.  If TRUE, will use "condensed" matrix notation 
#' @param onlyDiag [L,1] Logical flag. If TRUE, will only obtain the diagonal of the matrix
#' @return A matrix with the contents of the matrix described by txt 
#' @note Based on code by R. Francois
#' @author Mango Solutions
#' @export
.importNmLstMatrix <- function( 
		txt,                  #@ [C+] text in which to read the matrices
		condensed = NULL,          #@ [L,1] using the condensed notation
		#txt %a~% "\\|" , 
		onlyDiag = FALSE      #@ [L,1] Only the diagonal of the matrix is required
)
{
	# detect use of condensed notation
	if(is.null(condensed))
		condensed <- any(regexMatches(txt, rx = "\\|"))
	
	
	if( is.null(txt)) 
	{	
		
		return(NULL)  
	}
	
	### find the variable names 
	# lines matching that regex contain variable names, and lines not matching 
	# contain numbers to import
	varRx <- if( condensed ) "\\|"
			else "^[[:space:]]*[[:alpha:]]+[[:space:]]*[[:digit:]]+"	
	vars <- .nmVariableNames( grep( varRx, txt, value = TRUE) )
	
	### extract the relevant data
	dataTxt <- negGrep( varRx, txt, value = TRUE)            # only the lines with data
	dataTxt <- gsub( "^\\+", "", dataTxt )    # remove beginning of line plus
	dataTxt <- gsub( "\\.{2,}", "0", dataTxt) # replace ....... by 0  
	dataVal <- as.numeric( .readValues( dataTxt ) )
	
	### stucture the output into a matrix
	out <- matrix( 0, nrow = length(vars) , 
			ncol = length(vars), dimnames = rep(list(vars), 2) )
	out[ upper.tri(out, diag = TRUE) ] <- dataVal
	if( onlyDiag ) { 
		prefix <- gsub( "[[:digit:]]+", "", rownames(out)[1] ) # guess the prefix
		out <- diag( out )
		names(out) <- sprintf( "%s.%d.%d", prefix, 1:length(out), 1:length(out) )
	} else {
		out[ lower.tri( out ) ] <- t(out)[ lower.tri( out ) ]
	}
	out
} 