

#' Parses a NONMEM control file, and returns its contents as a list of parsed elements.  These elements
#' will correspond to the actual control statements, e.g. $PK, $THETA, $PROBLEM, etc.  Some of the elements
#' are kept as pure text.
#' @title Parse a NONMEM control file
#' @param fileName File name name of the control file
#' @param path (optional) path to the control file, can be a stored path enclosed in round brackets
#' @param version NONMEM version that the control file targets.  Currently, "VII" or "7" denotes NONMEM 7, and
#' other values are not differentiated 
#' @param textReport Logical flag.  If TRUE, a message regarding which file is being imported will be logged.
#' @title Import a control file
#' @return A list with parsed components of the control file.  This will be of class \code{nmModel}.
#' @author Mango Solutions
#' @examples
#' \dontrun{
#'  conContents <- importNmMod(file = "theoph.con", path = "examples/theoph") 
#'  print(conContents)
#' }
#' @note An execption will be generated if $PRIOR NWPRI is found in a control file and NONMEM version VII was used. If the control file is empty, 
#' an exception will also be generated. 
#' @keywords IO

importNmMod <- function(
		fileName = NULL, path = NULL, version = "VI", textReport = FALSE )
{	
	# log messages
	if(textReport)
		logMessage(logName = "stdReport", paste("Importing file", fileName, "\n"))
	
	path <- processPath(path)
	### import the file or just read the character vector on textFile             
	fileContents <- scanFile(.getFile(fileName, path))
	# check if the file contents were returned as NULL, and return NULL
	
	if( is.null(fileContents) ) 	
		RNMImportStop(paste("Contents of the file", fileName, "are empty \n"), match.call())
	
	# remove lines that only contains spaces and semicolons
	fileContents <- negGrep( "^[;[:space:]]+$", fileContents, value = TRUE)
	
	#Strip out the whitespace from the beginning of each line.  This avoids many potential problems
	fileContents <- killRegex(fileContents, "^[[:blank:]]*") 
	
	# remove comments.  Currently they are not used anywhere, but might facilitate certain operations
	comments <-  commentPop(fileContents, inPlace = FALSE)$op.out
	
	# split the text by problem		
	problemTexts <- partitionByProblem(fileContents)
	
	numProblems <- length(problemTexts)
	
	msg <- paste(fileName, "has", numProblems, "problems")
	logMessage(logName = "highLevelParse", msg)
	
	# must have at least one problem
	if( numProblems == 0){
		RNMImportStop( paste("Cannot find $PROBLEM statement in control file", fileName), match.call() ) 
	}
		
	
	### create output list                                                        
	outList <- list(Raw = fileContents, Comments = comments)
	
	### Now we begin iteratively importing each problem	

	problemContents <- vector(mode = "list", length = numProblems)
	
	# store the required importing function
	if(version != "VII")
		.importSingleProb  <- .importNmModSingleProblem
	else
		.importSingleProb <- .importNmModSingleProblemNM7
	# first problem is dealt with normally, but additional ones have to be dealt with differently
	# TODO: The logic for handling the additional problems probably needs to be dealt with elsewhere.
	# For example, a simulation that follows a normal problem might need access to the THETAs from the previous
	# estimation (which are found in the report file)
	
	problemContents[[1]] <- .importSingleProb(problemTexts[[1]], fileName)
	
	for(i in seq_along(problemTexts)[-1])
	{
		contents <- .importSingleProb(problemTexts[[i]], fileName)
		# problemContents[[i]] <- .mergeMissing(contents, problemContents[[i-1]])
		problemContents[[i]] <- contents
		 
	}
	outList$controlFile <- .getFile(fileName, path)
	outList$problemContents <- problemContents 
	new("nmModel" , outList)  	
}

