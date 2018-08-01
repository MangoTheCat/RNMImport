#' Parses the $SUBROUTINE section of the NONMEM control file, and returns the information
#' as a character vector
#' @param txt - Contents of a control file possibly holding a $SUBROUTINE statement 
#' @param .extract 
#' @title Parse $SUBROUTINE statement
#' @return A character vector with the names of the subroutines used (ADVAN1, etc.)
#' @author Mango Solutions
#' @note Based on code originally by R Francois

.importNmModSub <- function(txt, .extract = length(grep("\\$SUB",txt)) > 0)
{
	out <- if(.extract) section( txt, "SUB", "", stripout = TRUE, glue = TRUE, as.list = FALSE ) else txt
	# extract "TOL= X"
	tol <- equalExpressionPop( out, "TOL", inPlace = TRUE )
	subroutines <- equalExpressionPop( out, "SUB", shortcut = TRUE, inPlace = TRUE)
	out <- stripBlanks( out )
	# extract the names of the individual subroutines
	out <- unlist( strsplit( out, "[[:space:]]+") )
	out	
}