

#' Reads a "vector" stored in the text of a list file
#' @param text Vector of strings of text of a list file
#' @return A named numeric vector
#' @author Mango Solutions
#' @export
.importNmLstVector <- function(text)
{
	
	# 
	# Import a "vector" structure from the output file
	
	if( is.null(text)) return(NULL) 
	ltext <- length(text)
	if( (ltext  %% 2) != 0)
		RNMImportStop("text length is not divisible by two", call = match.call())
	
	varsIndex <- 1:(ltext/2)   # first half
	# extract the variable names
	vars <- .nmVariableNames( text[ varsIndex] )  
	values <- as.numeric( .readValues( text[-varsIndex] ) )
	names( values ) <- vars
	values
}

