#' Parses the $SIM statement of a control file (should have comments removed), called internally by
#' importNmMod
#' @title Parse $SIM statement
#' @param txt [C,1] - Contents of a control file possibly holding a $SIM statement 
#' @param .extract 
#' @param file [C,1] - Optional filename where contents are to be extracted (deprecated) 
#' @return A character vector holding the number of simulation subproblems, the 2 seed values, and
#'  whether or not SIMONLY is TRUE or FALSE
#' @author Mango Solutions


.importNmModSim <- function(
	txt = NULL, 
	.extract = length(grep( "\\$SIM", txt )) > 0, 
	file = NULL
)
{	
	if(!is.null(file))
		txt <- scanFile(file)
	### extract the SIM section                                                   
	txt <- if(.extract) section( txt, "SIM", "", stripout = TRUE, as.list = FALSE) else txt
	
	# we wish to store this raw statement for later use
	
	simStatement <- txt
	### remove the unwanted NONMEM keywords                                       
	# determine whether this is data generating-only simulation
	simOnly <- any(regexMatches(txt, rx = "ONLY"))
	# TODO: correct the regular expression used to indicate simonly statements
	listToRemove <- c("NEW", "REQUESTFIRST", "REQUESTSECOND", 
			"NOPREDICTION", "PREDICTION", "OMITTED", "ONLYSIMULATION")
	txt <- killRegex( txt, listToRemove )
	### extract the seeds                                                         
	Seed1 <- bracketPop( txt, absent = -1, removeBrackets = TRUE, inPlace=TRUE)
	Seed2 <- bracketPop( txt, absent = -1, removeBrackets = TRUE, inPlace=TRUE)
	
	
	### find the number of subproblems                                            
	# using shorcut because PROB can be one of : 
	# NSUBPROBS, NSUBPROBLEMS, SUBPROBLEMS, NPROBLEMS, ...
	# another way is to use "N?S?U?B?PROBL?E?M?S" instead of "PROB"
	nSub <- equalExpressionPop( txt, "PROB", absent = 1, shortcut = TRUE, sep = "=", inPlace = TRUE )
	if( nSub == 1) nSub <- equalExpressionPop( txt, "SUB", absent = 1, shortcut = TRUE, inPlace = TRUE )
	
	### the TRUE option                                                           
	# TODO: is INTIAL the right default value if TRUE not given ?
	true <- equalExpressionPop( txt, "TRUE", absent = "INITIAL", inPlace = TRUE)
	
	### build the output structure                                                
	out <- c( "nSub" = nSub, "Seed1" = Seed1, "Seed2" = Seed2, "TRUE" = true, "simOnly" = simOnly )
	attr(out, "rawStatement") <- simStatement
	out 
	
}