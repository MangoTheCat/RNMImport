

# internal function meant to be used internally by importNmLstSimModel ONLY
.importSubProb <- function(txt)
{
	secs <- sectionLst(txt)
	objective <- .nmMVOF(secs[["MINIMUM VALUE OF OBJECTIVE FUNCTION"]])
	
	estimates <- .importNmLstEstimates(secs[["FINAL PARAMETER ESTIMATE"]])
	c(list("objectiveMin" = objective), estimates)
}


#' Imports the contents of the report file of a SimModel type problem
#' @param contents Contents of the report file for a single problem
#' @param numSub Number of sub-problems in the problem 
#' @title Import simulation+fit report 
#' @return A list with parsed elements
#' @author Mango Solutions
#' 

importNmLstSimModel <- function(contents, numSub = NA)
{
	contents <- cleanReportContents(contents)
	if(is.na(numSub))
	{
		# find all lines of the form
		#PROBLEM NO.:         1    SUBPROBLEM NO.:      N
		subprobLines <- grep(contents, pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*[0-9]+[[:blank:]]*$")
		numSub <- if(length(subprobLines) >= 1) length(subprobLines) else 1
		
	}
	if(numSub > 1)
	{
		outList <- list()       
		
		# outList$VersionInfo  <- nmVersion( contents ) 
		
		# extract the number of records and individuals in the data
		outList$nRecords     <- colonPop( contents, "TOT\\. NO\\. OF OBS RECS"   , inPlace = FALSE, numeric = TRUE )$op.out
		outList$nIndividuals <- colonPop( contents, "TOT\\. NO\\. OF INDIVIDUALS", inPlace = FALSE, numeric = TRUE )$op.out
		# split off the part of the control file that has the subproblems
		# This line will look as follows (N is any integer greater than 1):
		#PROBLEM NO.:         N     SUBPROBLEM NO.:      1
		subprobStartLine <- 
			grep(contents,
			pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*1[[:blank:]]*$" )
		if(length(subprobStartLine) == 0 || length(subprobStartLine) > 1 )
			RNMImportStop("Not able to locate first subproblem in simulation lst file\n", call=match.call())
		subprobContents <- tail(contents, -subprobStartLine + 1)
		# cut the subproblems into chunks
		subprobStatements <- lapply(partitionLstSubproblems(subprobContents), .importSubProb)
		
		# get all objective function values
		objectives <- sapply(subprobStatements, "[[", "objectiveMin")
		names(objectives) <- paste("sim", sep = "", 1:numSub)
		# now insert the thetas into a matrix with one row for each subproblem. 
		thetas <- t(sapply(subprobStatements, function(x) x$THETA))
		rownames(thetas) <- paste("sim", sep = "", 1:numSub)
		
		tmp <- subprobStatements[[1]]$OMEGA
		# put the omegas and sigmas into a 3d array ("vector of matrices")
		omegas <- array(dim = c(dim(tmp), numSub), dimnames = list(rownames(tmp),colnames(tmp), 
							paste("sim", sep = "", 1:numSub)))
		tmp <- subprobStatements[[1]]$SIGMA
		sigmas <- array(dim = c(dim(tmp), numSub),dimnames = list(rownames(tmp),colnames(tmp), 
						paste("sim", sep = "", 1:numSub)))
		 
		for(i in 1:numSub) 
		{
			omegas[,,i] <- subprobStatements[[i]]$OMEGA
			sigmas[,,i] <- subprobStatements[[i]]$SIGMA
			
		}
		outList$FinalEstimates <- list(THETA = thetas, OMEGA = omegas, SIGMA = sigmas, Objective.Minimum = objectives)
		
	}
	else
	{
		# if there is only one subproblem, then the file looks virtually identical to a basic model list file
		outList <- .importNmLstBasicProb(contents)
		# however,we need to change the dimensions of some returned data
		outList$FinalEstimates$THETA <- matrix(outList$FinalEstimates$THETA, 
				nrow = 1, dimnames = list("sim1", names(outList$FinalEstimates$THETA)))
		
		x <- outList$FinalEstimates$OMEGA
		dim(x) <- c(dim(x), 1)
		dimnames(x) <- c(dimnames(outList$FinalEstimates$OMEGA), list("sim1"))
		outList$FinalEstimates$OMEGA <- x
		
		x <- outList$FinalEstimates$SIGMA
		dim(x) <- c(dim(x), 1)
		dimnames(x) <- c(dimnames(outList$FinalEstimates$SIGMA), list("sim1"))
		outList$FinalEstimates$SIGMA <- x
		
		
		names(outList$Objective.Minimum) <- "sim1"
	
	}
	outList	
}

#' Parses the results of a single BASIC MODEL
#' @param contents character vector of text for a single problem statement.
#' @title Import basic problem report results 
#' @return a list containing final estimates, number of individuals, etc. for the problem 
#' @author Mango Solutions

.importNmLstBasicProb <- function(contents)
{

	outList <- list() 
	outList$nRecords     <- colonPop( contents, "TOT\\. NO\\. OF OBS RECS"   , inPlace = FALSE, numeric = TRUE )$op.out
	outList$nIndividuals <- colonPop( contents, "TOT\\. NO\\. OF INDIVIDUALS", inPlace = FALSE, numeric = TRUE )$op.out
	
	### Find the sections of the list file
	lstList <- sectionLst( contents )
	
	### Extract minimum value of objective function  
	outList$Objective.Minimum <- .nmMVOF( lstList[["MINIMUM VALUE OF OBJECTIVE FUNCTION"]] )
	
	### Extract matrices
	outList$CovarianceMatrix        <- .importNmLstMatrix( lstList[["COVARIANCE MATRIX OF ESTIMATE"        ]] )
	outList$CorrelationMatrix       <- .importNmLstMatrix( lstList[["CORRELATION MATRIX OF ESTIMATE"       ]] )
	outList$InverseCovarianceMatrix <- .importNmLstMatrix( lstList[["INVERSE COVARIANCE MATRIX OF ESTIMATE"]] ) 
	### Extract Estimates   
	outList$initialEstimates        <- .importNmLstEstimates( lstList[["INITIAL PARAMETER ESTIMATE"]] )
	outList$FinalEstimates          <- .importNmLstEstimates( lstList[["FINAL PARAMETER ESTIMATE"]] )
	outList$StandardError           <- .importNmLstEstimates( lstList[["STANDARD ERROR OF ESTIMATE"]] )
	
	### Extract iteration information
	outList$Iter <- .importNmLstIter( lstList[["MONITORING OF SEARCH"]])
	
	outList
	
}

#' Imports the contents of a simulation data generation problem from a report file into a list
#' @param contents [C,+] Character vector of 
#' @title Import report file contents of a simulation data generation problem ( without model fitting step )
#' @return A list with 2 elements: nRecords and nIndividuals, holding the number of individuals and number of
#' records in the problem respectively
#' @author Mango Solutions
#' 

importNmLstSimDataGen <- function(contents)
{
	outList <- list() 
	outList$nRecords     <- colonPop( contents, "TOT\\. NO\\. OF OBS RECS"   , inPlace = FALSE, numeric = TRUE )$op.out
	outList$nIndividuals <- colonPop( contents, "TOT\\. NO\\. OF INDIVIDUALS", inPlace = FALSE, numeric = TRUE )$op.out
	outList
}


# Takes the content of a report file and splits into a list of problems

.reportPartitionedByProblems <- function(reportContents, numProblems = NA)
{
	# only one problem, no partitioning necessary
	
	problemDelimeterRegexp <- "^[[:blank:]]*PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*$"
	probStartPoints <- grep(reportContents, pattern = problemDelimeterRegexp)
	if(is.na(numProblems)) numProblems <- length(probStartPoints)
	RNMImportStopifnot(length(probStartPoints) == numProblems, "Number of problems specified does not match actual number of problems found in report")

	if(numProblems == 1)
		return(list(reportContents))	
	individualProblemReports <- vector(mode = "list", length = numProblems)
	for(i in seq_along(probStartPoints[-1]) )
	{
		individualProblemReports[[i]] <- reportContents[probStartPoints[i]:(probStartPoints[i+1] - 1)]
		
	}
	individualProblemReports[[numProblems]] <- tail(reportContents, n = - probStartPoints[numProblems] + 1)
	individualProblemReports
}

#' Imports a NONMEM report file based on information contained in control statements corresponding to some problem
#' @param fileName Name of the report file to import
#' @param controlStatements 
#' @title Import report file based on control statements
#' @return List of parsed report file statements
#' @author Mango Solutions
#' 

# TODO: suppress this?  It should be possible to fully import a list file without a control file, and then there
# would be no sense in maintaining this function

.importNmLstWithCtl <- function(fileName, controlStatements )
{
	content <- scanFile(fileName)
	if( is.null(content) )  {
		RNMImportWarning(paste("Contents of the list file", fileName, "were empty or read incorrectly"))
		return(NULL)
	}
	
	result <- list(Raw = content)
	# clean the report contents for easier parsing
	cont <- cleanReportContents(content)
	
	# Capture the version info - this should not be repeated for each problem
	result$VersionInfo <- nmVersion( cont )
	allProbContents <- controlStatements$problemContents
	partitionedContent <- .reportPartitionedByProblems(content, length(allProbContents))
	
	problems <- vector(mode = "list", length = length(partitionedContent))
	
	# if(length(controlStatements$problemContents) > 1)
	#	RNMImportStop("Cannot yet import lst files with more than one problem", call = match.call())
	for(i in seq_along(partitionedContent)) {
		probContent <- allProbContents[[i]]
		contents <- partitionedContent[[i]]
		isSim <- !is.null(probContent$Sim)	
		# this is a basic problem (no simulation, one problem statement only)
		if(!isSim)
			problems[[i]] <- .importNmLstBasicProb(contents)
		# single simulation problem
		if(isSim)
		{
			# check if it is SIMONLY, if so, we're not interested in the list file
			if(probContent$Sim["simOnly"] == "TRUE")
			{
				logMessage(logName = "detailedReport", "Sim only report file, contents will be disregarded\n")
				problems[[i]] <- character(0)
			}
			else
			{
				problems[[i]] <- importNmLstSimModel(content, as.numeric(probContent$Sim["nSub"]))
			}
		
		}
		
	}
		
	result$problemResults <- problems
	
	new("nmRunReport", result)
}

#' This routine imports the contents in a NONMEM output report file, and then parses different sections of it
#' while ignoring some. A structured list object is returned
#' @title Import the information in an output report file
#' @param fileName Name of the report file
#' @param path The path where the report file is held. Can be a path stored by setNmPath if it is in round brackets. 
#' @param controlStatements (optional) Obh Control statements from the run associated with this output.  Using it 
#' allows cleaner parsing of the report file.  This will likely be deprecated in the future.  
#' @param textReport Single logical.  Should statements be logged to "stdReport" log?
#' @return A named report with elements holding parsed statements in the report file.
#' This object will be of class \code{nmRunReport}
#' @author Mango Solutions
#' @keywords IO
#' @examples 
#' \dontrun{
#'      x <- importNmReport("theoph.lst", path = "examples/theoph")
#'      print(x)
#' }

importNmReport <- function( fileName, path = NULL, controlStatements = NULL, textReport = FALSE)
{          
	logMessage("Importing the lst file " %pst% fileName %pst% "\n", logName = "highLevelParse")
	
	# check for version of NONMEM, and call another funciton if necessary.  Not the cleanest possible solution...
	
	# use getNmPath if necessary
	path <- processPath(path)
	
	if(!is.null(controlStatements))
		return(.importNmLstWithCtl(.getFile(fileName, path ), controlStatements) )
	
	logMessage(logName = "highLevelParse", "importNmReport: No control statements found...\n")
	content <- scanFile(.getFile(fileName, path) )	
	
	versionInfo <- nmVersion(content)
	# check if the version is NONMEM 7.  If so, import with importNmReport.NM7
	if(substr(versionInfo[1], start = 1, stop = 1) == "7")
		return(importNmReport.NM7(content, textReport = textReport))
	
	# if content is NULL, return NULL
	
	if( is.null(content) )  {
		RNMImportWarning(paste("Contents of the list file", fileName, "were empty or read incorrectly"))
		return(NULL)
	}
	result <- list(Raw = content)
	content <- cleanReportContents(content)
	
	# Capture the version info - this should not be repeated for each problem
	result$VersionInfo <- nmVersion( content )
	# allProbContents <- controlStatements$problemContents
	partitionedContent <- .reportPartitionedByProblems(content)
	
	# now loop through the different problems
	
	problemResults <- vector(mode = "list", length = length(partitionedContent))	
	for(i in seq_along(problemResults))
	{
		currentProb <- partitionedContent[[i]]
		# check for the presence of  SIMULATION STEP PERFORMED
		simStep <- any(regexMatches("SIMULATION STEP PERFORMED", txt= currentProb))
		# check for value of objective function
		objFun <- any(regexMatches("MINIMUM VALUE OF OBJECTIVE FUNCTION", txt = currentProb))
		# simulation + model
		if(simStep & objFun)
		{	
			if(textReport)
				logMessage(logName = "stdReport", "Appears to be a simulation+modelling problem\n")
			problemResults[[i]] <- importNmLstSimModel(currentProb, NA)
		}
		# only data simulation, no fit step
		else if(simStep & !objFun)
		{	
			problemResults[[i]] <- importNmLstSimDataGen(currentProb)
		}
		else
		{
			if(textReport)
				logMessage(logName = "stdReport", "Appears to be a standard model\n")
			problemResults[[i]] <- .importNmLstBasicProb(currentProb)
			
		}
	}
	result$problemResults <- problemResults
	new("nmRunReport", result)
}

#' Removes extraneous content from the report file text
#' @param contents Lines of the report file
#' @title Clean report file
#' @return character vector of strings without extraneous content
#' @author Mango Solutions
#' 

cleanReportContents <- function(content)
{
	# Remove ERROR messages and all content that follows
	grepErrors <- grep("1THERE ARE ERROR MESSAGES", content)
	if (length(grepErrors)) content <- content[1:(min(grepErrors)-1)]
	
	# fgochez: change on Feb 18 2009
	# strip out the everything up until the version of NONMEM used, as it is extraneous as well
	startLine <- grep("1NONLINEAR MIXED EFFECTS MODEL", content)
	if(length(startLine))
	{
		logMessage(logName = "lowLevelParse", 
				paste("Discarding report file lines up until line:", min(startLine) - 1,"\n"))
		if(min(startLine) > 1)
			content <- tail(content, -(min(startLine) - 1))
	}
		
	# Remove final statements and onwards
	grepFinal <- grep("^This file was created", content)
	
	if (length(grepFinal)) 
	{
		logMessage(logName = "lowLevelParse", "Stripping out the final lines from " %pst% (min(grepFinal) - 1))
		content <- content[1:(min(grepFinal)-1)]
	}
	
	#################
	# WARNING: the following code is somewhat dubious, but is necessary for dealing with the footers
	# of certain lst files.  In some cases, footers such as the following occur:
	# Stop Time: 
	# Tue 01/26/2010 
	# 11:17 AM
	
	unusualFooterLine <- grep(content, pattern = "[Ss]top [Tt]ime" )
	if(length(unusualFooterLine) > 0)
		content <- head(content, n = unusualFooterLine[1] - 1)
	
	# strip out lines containing a date/time of execution, e.g.
	# "Thu Jan 14 13:03:59 GMT 2010" 
	negGrep(content, value = TRUE, 
			pattern = "^[A-Z][a-z][a-z][[:space:]][A-Z][a-z][a-z][[:space:]][0-9]{1,2}[[:space:]][0-9]{2}\\:[0-9]{2}")
}