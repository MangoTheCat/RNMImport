
#' Constructor for the NMSimModelNM7 class
#' @title create NONMEM 7 simulation+fitting class
#' @param controlStatements Set of parsed control statements 
#' @param path Path where the run is located
#' @param reportContents Parsed contents of the report file
#' @return Newly constructed object of class NMSimModelNM7
#' @author Mango Solutions

NMSimModelNM7 <- function(controlStatements, path, reportContents, versionInfo = c("major" = "VII", "minor" = 1))
{
	inData <- try(importModelData(dataStatement = controlStatements$Data,inputStatement = controlStatements$Input, path = path))
	# if we could not read data file for some reason, continue anyway
	if(inherits(inData, "try-error"))
	{
		msg <- paste("Could not import data file.  Error generated was:",
				inData, "\nWill continue importing other components\n")
		RNMImportWarning(msg)
		inData <- data.frame()
	} # end if(inherits(inData, "try-error"))

	outTables <- .importTablesSafely(controlStatements$Table, path = path  )
	# if the output tables are a "list", then there was a FIRSTONLY statment, or for some other reason
	# the number of rows of all of the output tables were not equivalent
	if(inherits(outTables, "list")) nDataRows <- max(sapply(outTables, nrow))
	else nDataRows <- nrow(outTables)
	seeds <- as.numeric(ifelse(controlStatements$Sim[c("Seed1", "Seed2")] == -1, NA,	
					controlStatements$Sim[c("Seed1", "Seed2")]))
	nSim <- as.numeric(controlStatements$Sim["nSub"])
	
	with(reportContents, 
		{	
			objectiveFinal <- Objective.Minimum
			
			numMethods <- nrow(controlStatements$Estimates)
			
			# when the report/list file is read in, these are grouped by simulation rather than by method, as we want
			# thus we rearrange them
			
			omegaFinal <- .rearrangeEstimates(FinalEstimates$OMEGA, nSim, numMethods)
			sigmaFinal <- if(!is.null(FinalEstimates$SIGMA)) .rearrangeEstimates(FinalEstimates$SIGMA, 
								nSim, numMethods) else list()
			thetaFinal <- aperm(FinalEstimates$THETA, perm = c(3, 2, 1))
						
			new("NMSimModelNM7", numSimulations = nSim, 
					seeds = seeds, inputData = inData, outputData = outTables, controlStatements = 
							controlStatements, problemStatement = controlStatements$Problem,
					numMethods = numMethods,
					# TODO: the following is not correct because method names from the list file are not being stored!
					methodNames = reportContents$methodNames,
					thetaInitial = controlStatements$Theta[,"Est"], 
					omegaInitial = controlStatements$Omega, 
					sigmaInitial = controlStatements$Sigma,
					omegaFinal = omegaFinal,
					sigmaFinal = sigmaFinal,
					thetaFinal = thetaFinal,
					objectiveFinal = objectiveFinal,
					additionalVars = as.data.frame(matrix(ncol = 0, nrow = nDataRows)),
					methodInfo =  controlStatements$Estimates ,
					nmVersionMajor = versionInfo["major"],
					nmVersionMinor = as.numeric(versionInfo["minor"]),
					reportStatements = reportContents)
		})
}

#' Utility function which rearranges the SIGMA and OMEGA parameter estimates retrieved by importNmReport
#' @param matrixList List of matrices of SIGMAs or OMEGAs as imported from a NONMEM 7 list file by importNmReport
#' @param nSim Number of simulations performed
#' @param numMethods Number of methods used
#' @title Rearrange parameter estimates
#' @return A list of estimates by method used


.rearrangeEstimates <- function(matrixList, nSim, numMethods)
{
	matrixDim <- dim(matrixList[[1]])[1:2] 
	
	marginNames <- dimnames(matrixList[[1]])[1:2]
	
	transformedList <- vector(length = numMethods, mode = "list")
	for(i in seq_along(transformedList))
	{
		# extract the results for one method only
		currentMethodResults <- lapply( matrixList, function(x) as.matrix(x[,,i]) )
		
		currentMethodResultsArray <- array(NA, dim = c(matrixDim, nSim ), 
				dimnames = c( marginNames, list( names(currentMethodResults) ) ) )
		
		for(j in seq_along(currentMethodResults))
		{
			currentMethodResultsArray[,,j] <- currentMethodResults[[j]]
		}
		transformedList[[i]] <- currentMethodResultsArray
		# this will be a list of matrices, which we wish to turn into an array
		
	} 
	transformedList
}
