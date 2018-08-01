
#' Imports a single NONMEM problem into a list structure
#' @name .importNmModSingleProblem
#' @title Import Single NONMEM problem
#' @param contents Text of the contents of a SINGLE problem statement
#' @param fileName Name of the control file from which the problem originates
#' @return named list with the various sections split up
#' @author Mango Solutions
#' @export
#' 

.importNmModSingleProblemNM7 <- function(contents, fileName)
{

	# retrieve a version of the text without comments
	poppedTxt <- commentPop(contents, inPlace = FALSE)$txt
	prob <- list()	
	# find the existent sections in the current problem
	titles <- sectionTitles(poppedTxt)
	titles <- unique(toupper(titles))
	if(.lookFor(contStates=titles, subr=poppedTxt[grep('\\$SUB.*', poppedTxt)], pri='PRI|OR'))
	{	
		logMessage(logName = "lowLevelParse", "$PRIOR found \n")
		prob$Prior <- .importNmModPrior( contents )
# 		handle case with priors here, else ignore
#		if(attr(prob$Prior, "NWPRI") == TRUE)
#			RNMImportStop("$PRIOR NWPRI statement detected.  Importing of this is not supported, so will halt.\n", match.call())
		
	}
	# deal with parameters
	prob$Theta <- if( "THE" %in% titles | "THT" %in% titles ) .importNmModTheta( txt=contents )
	prob$Omega <- if("OME" %in% titles ) .importNmModOmega( txt=contents, component = "OMEGA")
	prob$Sigma <- if( "SIG" %in% titles ) .importNmModOmega( txt=contents, component = "SIGMA" )
	# extract any raw FORTRAN code
	prob$Script <- fortranPop(poppedTxt, inPlace = TRUE)
	
	# now extract the $PROB statement   
    probstat <- section(poppedTxt, "PRO", "", stripout = TRUE, as.list = FALSE, glue = TRUE, clean = TRUE)
    if (length(probstat)<1) {
        # maybe $PROBLEM ; XXXXX
        probstat = section(contents, "PRO", "", stripout = TRUE, as.list = FALSE, glue = TRUE, clean = TRUE)
    }
    if (length(probstat)<1) {
        # if probstat is character(0), the dmpSummary will fail
        probstat = '<NO PROBELM STATEMENT specified in Control Stream>'
    }
    prob$Problem <- probstat
	# extract $TABLE
	prob$Tables <- if( "TAB" %in% titles ) .importNmModTables( poppedTxt) 
	# extract $SUB
	prob$Subroutine <- if( "SUB" %in% titles ) .importNmModSub( poppedTxt)
	# $INPUT statement
	prob$Input <- if( "INP" %in% titles ) .importNmModInput( poppedTxt )
	# TODO: the fact that fileName was not passed down here was not generating any problems.  Why?
	prob$Data <- if( "DAT" %in% titles ) .importNmModData( poppedTxt, fileName )
	prob$Sim <- if("SIM" %in% titles ) .importNmModSim( poppedTxt )
	
	
	# From now on, simply extract raw text for the other sections
	
	### extract the PK model                                                      
	prob$PK <- section( poppedTxt, "PK", "", as.list = FALSE, stripout = TRUE)
	
	### extract the PRED model                                                    
	prob$PRED <- section(poppedTxt, "PRED", "", as.list = FALSE, stripout = TRUE)
	
	### extract the Model                                                         
	prob$Model <- section( poppedTxt, "MOD", "", as.list = FALSE, stripout = TRUE)
	
	### extract the Error statements                                              
	prob$Error <- section( poppedTxt, "ERR", "", as.list = FALSE, stripout = TRUE)
	
	### extract the Mix statements                                                
	prob$Mix <- section( poppedTxt, "MIX", "", as.list = FALSE,
			stripout = TRUE, clean = TRUE)
	
	### extract the EST statements                                                
	
	if("EST" %in% titles) prob$Estimates <- .importNmModEst(poppedTxt)
	
	### extract the COV statements                                                
	prob$Cov <- section( poppedTxt, "COV", "", glue = TRUE,  
			as.list = FALSE, stripout = TRUE, clean = TRUE)
	prob
}
