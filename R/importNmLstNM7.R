

#' This function parses
#' @param methodTextBlock 
#' @title Parse report file #METH text block
#' @return block result
#' @author Mango Solutions
#' @export
.importMethodBlock <- function(methodTextBlock, SIGDIGS71='Sig.digs|SIG. DIGITS')
{
	blockResult <- list(  )
	blockResult$method <- attr(methodTextBlock, "method.name")

	# retrieve objective function value
	
	objFinalLine <- grep(methodTextBlock, pattern = "#OBJV", value = TRUE)
	if(length(objFinalLine) < 1){
		message <- paste('No #OBJV tag found for', attr(methodTextBlock, "method.name"),
				'in .importMethodBlock')
		warning(message, call. = FALSE, immediate. = TRUE )
		blockResult$TermStatus <- message
		blockResult$Objective.Final <- NA
		return(blockResult)
	}
	objFinalValueLoc <- gregexpr(objFinalLine, pattern = "-{0,1}[0-9\\.]+")
	
	objFinalValue <- as.numeric( substr(objFinalLine, 
					start = objFinalValueLoc[[1]], 
					stop = objFinalValueLoc[[1]] + attr(objFinalValueLoc[[1]], "match.length") - 1 ) )
	
	# retrieve termination status
	termStatusLineNum <- grep(methodTextBlock, pattern = "#TERM")[1] + 1

	# termStatusFinalLineNum <- grep(tail(methodTextBlock, -termStatusLineNum), pattern = "^[[:space:]]*$")[1]
	blockResult$TermStatus <- gsub(methodTextBlock[termStatusLineNum], pattern = "^[[:space:]]+", replacement = "")
	
	# retrieve Sig.digs
	Sig.digsLineNum <- grep(methodTextBlock, pattern = SIGDIGS71)[1]
	if(!is.na(Sig.digsLineNum)){
		Sig.digsLoc <- 
				gregexpr(methodTextBlock[Sig.digsLineNum], pattern = "[0-9]+[.]+[0-9]*")[[1]]
		Sig.digs <- substr(methodTextBlock[Sig.digsLineNum], 
				start = Sig.digsLoc, 
				stop = Sig.digsLoc + attr(Sig.digsLoc, "match.length") - 1 )
#		remove leading white space
	} else {
		Sig.digs <- ' '
	}
	blockResult$Sig.digs <- Sig.digs

	Cov.statLineNum <- grep(methodTextBlock, pattern = " #TERE:")[1]
	Cov.stat <- ' '
	if(!is.na(Cov.statLineNum)){
		if(substr(methodTextBlock[Cov.statLineNum + 1], 1, 1)=='0')
			Cov.stat <- substring(methodTextBlock[Cov.statLineNum + 1], 2)
	}
	blockResult$Cov.stat <- Cov.stat
	
	# retrieve shrink values
	# TODO: does this work will many ETAs, or might there be issues with the way the text is wrapped?
	
	ETAshrinkLine <- grep("ETAshrink\\(%\\)", x = methodTextBlock, value = TRUE)
	if(length(ETAshrinkLine) > 0)
	{
		ETAshrinkText <- strsplit(ETAshrinkLine, split = ":[[:space:]]*")[[1]][2]
		blockResult$ETAshrink <- as.numeric(.readValues( ETAshrinkText, what = numeric(0) ))
	}
	else blockResult$ETAshrink <- NULL
	EPSshrinkLine <- grep("EPSshrink\\(%\\)", x = methodTextBlock, value = TRUE)
	if(length(EPSshrinkLine))
	{	
		EPSshrinkText <- strsplit(EPSshrinkLine, split = ":[[:space:]]*")[[1]][2]
		blockResult$EPSshrink <- as.numeric(.readValues( EPSshrinkText, what = numeric(0) ))
	}
	else
		blockResult$EPSshrink <- NULL
	blockResult$Objective.Final <- objFinalValue
	methodTextBlockSectioned <- sectionMethodBlock(methodTextBlock)
	blockResult$FinalEstimates <- .importNmLstEstimates(methodTextBlockSectioned$"FINAL PARAMETER ESTIMATE")
	blockResult$StandardError <- .importNmLstEstimates( methodTextBlockSectioned$"STANDARD ERROR OF ESTIMATE" )
	
	blockResult$CovarianceMatrix <- .importNmLstMatrix( methodTextBlockSectioned$"COVARIANCE MATRIX OF ESTIMATE" )
	blockResult$CorrelationMatrix <- .importNmLstMatrix( methodTextBlockSectioned$"CORRELATION MATRIX OF ESTIMATE" )

	blockResult$InverseCovarianceMatrix <- .importNmLstMatrix( methodTextBlockSectioned$"INVERSE COVARIANCE MATRIX OF ESTIMATE" )
	
	blockResult
}

#' Parses the contents of a NONMEM 7 report file into a list of parsed components 
#' (see the design info for details on the return structure)
#' @param content [C,+] The report file's text. 
#' @param textReport [L,1] Should standard text messages be logged to the stdReport log?
#' @title Import NONMEM 7 report file
#' @return A list with various parsed components of the report file.
#' @author Mango Solutions

importNmReport.NM7 <- function( content, textReport = FALSE )
{
	if( is.null(content) )  {
		RNMImportWarning("Contents of the list file were empty or read incorrectly")
		return(NULL)
	}
	
	result <- list(Raw = content)
	# clean up report contents
	content <- cleanReportContents(content)
	
	# Capture the version info - this should not be repeated for each problem
	versionInfo <- nmVersion( content )
	# for NONMEM 7, it seems that the version info is stored in the form 7.MINOR.X.  This we must further manipulate
	# the string to obtain major and minor versions
	versionInfoSplit <- strsplit(versionInfo, split = "\\.")[[1]]
	version <- "VII"
	level <- paste(versionInfoSplit[2], versionInfoSplit[3], sep = ".")
	result$VersionInfo <- c("Version" = version, "Level" = level)
	
	partitionedContent <- .reportPartitionedByProblems(content)
	
	# each problem should have its own set of results
	
	problemResults <- vector(mode = "list", length = length(partitionedContent))
	for(i in seq_along(problemResults))
	{
		currentProb <- partitionedContent[[i]]
		# we force the final line to be a "1", otherwise the  method block WILL NOT be parsed correctly. 
		# TODO: Find a more elegant way of handling this
		if(tail(currentProb, 1) != "1")
			currentProb <- c(currentProb, "1")
		
		# check for the presence of  SIMULATION STEP PERFORMED
		simStep <- any(regexMatches("SIMULATION STEP PERFORMED", txt= currentProb))
		# check for value of objective function
		objFun <- any(regexMatches("OBJECTIVE FUNCTION EVALUATION|MINIMUM VALUE OF OBJECTIVE FUNCTION", txt = currentProb))
		# simulation + model
		if(simStep & objFun)
		{	
		#	RNMImportStop("Simulations + fitting problems for NONMEM 7 not yet imported")
			if(textReport)
				logMessage(logName = "stdReport", "Appears to be a simulation+modelling problem\n")
			problemResults[[i]] <- importNmLstSimModel.NM7(contents=currentProb, numSub=NA)
		}
		# only data simulation, no fit step
		else if(simStep & !objFun)
		{	
			RNMImportWarning( "This is a simulation without modelling step, will only return raw contents\n", match.call() )
            problemResults[[i]] <- importNmLstSimOnly.NM7(contents=currentProb, numSub=NA)
		}
		# no data simulation,  EST step (JJ)
		else if(!simStep & objFun)
		{	
#			browser()
			problemResults[[i]] <- 
					.importNmLstBasicProb.NM7(contents=currentProb)
		}
		else
		{
			if(textReport)
				logMessage(logName = "stdReport", "Appears to be a standard model\n")
			problemResults[[i]] <- .importNmLstBasicProb.NM7(currentProb)
		}
	}
	result$problemResults <- problemResults
	new("nmRunReport", result)
}

#' Parses the results of a single BASIC MODEL
#' @param contents character vector of text for a single problem statement.
#' @title Import basic problem report results 
#' @return a list containing final estimates, number of individuals, etc. for the problem 
#' @author Mango Solutions
#' 

.importNmLstBasicProb.NM7 <- function(contents)
{
	
	# extract number of records and individuals
	outList <- list() 
	outList$nRecords     <- colonPop( contents, "TOT\\. NO\\. OF OBS RECS"   , inPlace = FALSE, numeric = TRUE )$op.out
	outList$nIndividuals <- colonPop( contents, "TOT\\. NO\\. OF INDIVIDUALS", inPlace = FALSE, numeric = TRUE )$op.out
	
	methodBlocks <- partitionMethods(contents)
	methodResults <- lapply( methodBlocks, .importMethodBlock)
	outList$MethodResults <- methodResults
	
	### Find the sections of the list file
	lstList <- sectionLst( fileContents=contents )
	searches <- which(names(lstList)=="MONITORING OF SEARCH")
	### Extract iteration information
	outList$Iter <- 
			lapply(searches, function(X, lst){
						.importNmLstIter(iterList=lst[[X]])	
					}, lst=lstList)
	
	outList
	
}

#' Imports the contents of the report file of a SimModel type problem compiled with NONMEM 7
#' @param [C,+] contents Contents of the report file for a single problem
#' @param [N, 1] numSub Number of sub-problems in the problem 
#' @title import NONMEM7 simulation+model fit problem report contents
#' @return out list
#' @author Mango Solutions
#' 

importNmLstSimModel.NM7 <- function(contents, numSub = NA)
{
	contents <- cleanReportContents(contents)
	if(is.na(numSub))
	{
		# find all lines of the form
		#PROBLEM NO.:         1    SUBPROBLEM NO.:      N
		subprobLines <- grep(contents, pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*[0-9]+[[:blank:]]*$")
		# if there is only one sub-problem, then the above line will not appear, hence the need for the following
		# logic 
		numSub <- if(length(subprobLines) >= 1) length(subprobLines) else 1
		
	}
#	if(numSub > 0)
#	{
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
	subprobContents <- partitionLstSubproblems(subprobContents)
	
	# import the contents of the individual sub-problems
	subprobStatements <- lapply(subprobContents, .importSubProbNM7)
	
	# get all objective function values for each method and sub-problem.  Structure these into a matrix
	# with one row for each sub-problem, and one column for each method
	
	numMethods <- length(subprobStatements[[1]]$MethodResults)
	numThetas <- length(subprobStatements[[1]]$MethodResults[[1]]$FinalEstimates$THETA)
	omegaDim <- dim(subprobStatements[[1]]$MethodResults[[1]]$FinalEstimates$OMEGA)
	
	objectiveMatrix <- matrix(NA, nrow = numSub, ncol = numMethods)
	
	# thetas will be structured into a 3d arrays, with one row for for each method, 
	# and one column for each theta,  
	# and one sub-matrix for each simulation problems
	
	thetaArray <- array( NA, dim = c(numThetas, numMethods, numSub ) )
	
	# omegas and sigmas will be structured into lists of 3d arrays, with a submatrix for each method result, and one list
	# element for each subproblem
	omegaList <- vector(mode = "list", length = numSub )
	sigmaList <- vector(mode = "list", length = numSub )
	# extract all estimates and objective function values
	
	methodLabels <- paste("METHOD", 1:numMethods, sep = "")
	for(i in seq_along(subprobStatements) )
	{
		objectiveFinals <- sapply(subprobStatements[[i]]$MethodResults, "[[", "Objective.Final" )
		objectiveMatrix[i,] <- objectiveFinals
		thetaFinals <- sapply(subprobStatements[[i]]$MethodResults, function(x) x$FinalEstimates$THETA)
		thetaArray[,,i] <- thetaFinals
		
		omegaFinals <- lapply(subprobStatements[[i]]$MethodResults, function(x) x$FinalEstimates$OMEGA)
		omegaFinals <- arrayFromMatrixList(omegaFinals, methodLabels)
		sigmaFinals <- lapply(subprobStatements[[i]]$MethodResults, function(x) x$FinalEstimates$SIGMA)
		sigmaFinals <- arrayFromMatrixList(sigmaFinals, methodLabels)
		
		sigmaList[[i]] <- sigmaFinals
		omegaList[[i]] <- omegaFinals
	}
	
	dimnames(objectiveMatrix) <- list(paste("sim", 1:numSub, sep = ""), paste("METH", seq_along(subprobStatements[[1]]$MethodResults), sep ="") )
	
	# now insert the thetas into a matrix with one row for each subproblem. 
	# thetas <- t(sapply(subprobStatements, function(x) x$THETA))
	
	simLabels <- paste("sim", sep = "", 1:numSub)
	
    thetaArray <- aperm(thetaArray, c(2,1,3))
	dimnames(thetaArray) <- list(methodLabels, names(subprobStatements[[1]]$MethodResults[[1]]$FinalEstimates$THETA)  ,simLabels)
	# give the list elements the names of the simulations from which they came
	names(omegaList) <- simLabels
	names(sigmaList) <- simLabels
	
	outList$FinalEstimates <- list(THETA = thetaArray, 
			OMEGA = omegaList, SIGMA = sigmaList)
	outList$Objective.Minimum <- objectiveMatrix
	outList$methodNames <- sapply(subprobStatements[[1]]$MethodResults, "[[", "method" )
	outList
}

# internal function meant to be used internally by importNmLstSimModel ONLY
.importSubProbNM7 <- function(txt)
{
	outList <- list() 
    if (length(grep('#METH', txt)) > 0) {
        methodBlocks <- partitionMethods(lstProblemContents=txt)
        methodResults <- lapply( methodBlocks, .importMethodBlock)
    } else {
        res <- block.parser(txt)
        if (length(res) == 1 && res[[1]]$type=='method.name') {
            attr(txt, 'method.name')  <- res[[1]]$val
            methodResults = list(.importMethodBlock( txt ))
        } else {
            RNMImportStop(msg = sprintf("Don't understand this text: \n\t %s \n", paste(txt,collapse='\n\t')))
        }
    }
	outList$MethodResults <- methodResults
	outList
}

importNmLstSimOnly.NM7 <- function(contents, numSub = NA)
{
	contents <- cleanReportContents(contents)
	if(is.na(numSub))
	{
		# find all lines of the form
		#PROBLEM NO.:         1    SUBPROBLEM NO.:      N
		subprobLines <- grep(contents, pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*[0-9]+[[:blank:]]*$")
		# if there is only one sub-problem, then the above line will not appear, hence the need for the following
		# logic 
		numSub <- if(length(subprobLines) >= 1) length(subprobLines) else 1
		
	}
#	if(numSub > 0)
#	{
	outList <- list()       
	
	# outList$VersionInfo  <- nmVersion( contents ) 
	
	# extract the number of records and individuals in the data
	outList$nRecords     <- colonPop( contents, "TOT\\. NO\\. OF OBS RECS"   , inPlace = FALSE, numeric = TRUE )$op.out
	outList$nIndividuals <- colonPop( contents, "TOT\\. NO\\. OF INDIVIDUALS", inPlace = FALSE, numeric = TRUE )$op.out
	
	outList
}
