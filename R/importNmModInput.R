

#' Parses the $INPUT section of a NONMEM control file
#' @param txt Character vector of text containing an $INPUT statement, typically the 
#' contents of a control file
#' @param .extract Flag indicating whether this is the $INPUT section itself, or whether the section must first be
#' extracted 
#' @title Parse $INPUT statement
#' @return a 2 column matrix mapping the variables in the input
#' statement with the variables used by NONMEM, which can be different by setting
#' aliases, e.g. ID=SUBJ in the $INPUT statement.  Also some may be dropped
#' @author Mango Solutions
#' 


.importNmModInput <- function(txt, .extract = length(grep("\\$INPUT", toupper(txt))) > 0)
{
	
	
	### import the $INPUT section and collapse it into one line                   
	inputSec <- if( .extract ) section( txt, "INP", "", stripout = TRUE, 
						remove.comments = TRUE, as.list = FALSE, glue = TRUE ) else txt
	
	### split by spaces                                                           
	inputSec.split <- strsplit(inputSec, "[[:space:],]+" )[[1]]
	
	### deal with the labels                                                      
	# if the label is already there, ie: X=DROP, then don't do anything
	# otherwise the label should be the same as the variable: X becomes  X=X
	negGrep.out <- negGrep( "=", inputSec.split )
	inputSec.split[negGrep.out] <- sprintf("%s=%s", 
			inputSec.split[negGrep.out], inputSec.split[negGrep.out] )
	
	### structure the output into a matrix                                        
	out <- matrix( unlist( strsplit(inputSec.split, "=") ), ncol = 2, byrow = TRUE )
	dimnames(out) <- list( 1:nrow(out), c("nmName", "Label"))
	# TODO: check the names against the forbidden names (ETA1, ...)   see ?$INPUT
	# TODO: check the names against the structure they should have. from ?$DATA: 
	# 1-4 letters (A-Z) and numerals (0-9), but it must begin with a letter.
	out
	
}
