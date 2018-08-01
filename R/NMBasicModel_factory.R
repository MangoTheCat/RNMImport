

	
#' Constructs an NMBasicModel object from the control statements and output list statements that apply to it.
#' Meant to be used from within importNm
#' @title NMBasicModel constructor
#' @param controlStatements [list] A list of control file statements for this particular problem  
#' @param path [C,1] path parameter, passed directly to importModelData and importModelOutputTables
#' @param lstContents [list] contents of an lst file that apply to this problem
#' @param versionInfo [C, +] numeric vector that holds
#' @return An NMBasicModel object holding the problem information
#' @author Mango Solutions

NMBasicModel <- function(controlStatements, path, reportContents, dropInputColumns = FALSE, 
		versionInfo = c("major" = "VI", "minor" = 0))
{
	inData <- try(importModelData(dataStatement = controlStatements$Data,inputStatement = controlStatements$Input, path = path,
					dropCols = dropInputColumns))
	# if we could not read data file for some reason, continue anyway
	if(inherits(inData, "try-error"))
	{
		msg <- paste("Could not import data file.  Error generated was:",
				inData, "\nWill continue importing other components\n")
		inData <- data.frame()
	} # end if(inherits(inData, "try-error"))
	
	# import output tables if the $TABLE statement is present, else outdata is empty
	outTables <- .importTablesSafely(tableStatement=controlStatements$Table, path = path  )
	
	# need to know how many rows the data has, handle FIRSTONLY case here
	if(inherits(outTables, "list")) {
        nOutDataRows <- max(sapply(outTables, nrow))
    } else {
        nOutDataRows <- nrow(outTables)
    }
	nInDataRows <- nrow(inData)
	if(is.null(nInDataRows)|is.null(nOutDataRows)){
		msg <- 	paste("NULL rows of input or output data!\n")
		RNMImportWarning(msg, match.call())
		print(controlStatements$Table)
	} else {
        if(nInDataRows != nOutDataRows) {
			msg <- 	paste("Number of rows of output data", nOutDataRows, 
					"\ndoes not match the number of rows of input data", nInDataRows,
					"!\n")
			cat(msg)
			RNMImportWarning(msg)
		}
    }
	# now create the class
	# TODO: The following is too complex, simplify in future releases
	with(reportContents,
			{

				# check for the covariance/correlation matrices
				covMatrix <- if(!is.null(reportContents$CovarianceMatrix)) CovarianceMatrix else matrix(ncol = 0, nrow = 0)
				corMatrix <- if(!is.null(reportContents$CorrelationMatrix)) CorrelationMatrix else matrix(ncol = 0, nrow = 0)
				# grab parameter initial values
				thetaInitial <- t(controlStatements$Theta)
				# these may be missing in the control statements, so try to extract them from the reportContents
				omegaInitial <- if(!is.null(controlStatements$Omega)) controlStatements$Omega  else  reportContents$initialEstimates$OMEGA
				# grab dimensions of omega final estimates
				omegaDim <- dim(FinalEstimates$OMEGA)
				# if no initial omega, fall back on a default set of names
				if(is.null(omegaInitial)) {
					omegaInitial <- matrix(NA, nrow = omegaDim[1], ncol = omegaDim[2])
					omegaDimNames <- list(paste( "OMEGA", 1:omegaDim[1], sep = "" ), paste( "OMEGA", 1:omegaDim[2], sep = "" ))
				}
				
				else omegaDimNames <- dimnames(omegaInitial)
				
				sigmaInitial <- controlStatements$Sigma
				if(is.null(sigmaInitial)) sigmaInitial <- matrix()
				rownames(thetaInitial) <- c("lowerBound", "initial", "upperBound")
				
				# if standard errors are available in the lst file,store them with the "XXXFinal" slots
				# TODO: In the future, recommend changing this logic and method of storage as this is getting 
				# quite convoluted
				if(!is.null(reportContents$StandardError))
				{
					# browser()
					thetaFinal <-  rbind(StandardError$THETA, FinalEstimates$THETA )
					
					rownames(thetaFinal) <- c("standardErrors","estimates")
				
					omegaFinal <- array(dim = c(omegaDim, 2), 
							dimnames = c(omegaDimNames, list(c("estimates", "standardErrors"))))
					omegaFinal[,,"estimates"] <- FinalEstimates$OMEGA
					omegaFinal[,,"standardErrors"] <- StandardError$OMEGA

					sigmaDim <- dim(FinalEstimates$SIGMA)
					# SIGMA can be omitted
					if(is.null(sigmaDim))
					{
						sigmaFinal <- array(dim = c(0,0, 2), dimnames = list(NULL,NULL,
										c("estimates", "standardErrors")))
					}
					else
					{
						sigmaFinal <- array(dim = c(sigmaDim, 2), dimnames = c(dimnames(sigmaInitial),
							list(c("estimates", "standardErrors"))))
						sigmaFinal[,,"estimates"] <- FinalEstimates$SIGMA
						sigmaFinal[,,"standardErrors"] <- StandardError$SIGMA
					}
				}
				else
				{
					thetaFinal <- matrix(FinalEstimates$THETA, nrow = 1, dimnames = list( "estimates" , NULL ))
										
					omegaFinal <- array(FinalEstimates$OMEGA, dim = c(omegaDim, 1),
						dimnames = c(dimnames(omegaInitial), list("estimates")))
					
					sigmaDim <- dim(FinalEstimates$SIGMA)					
					# if missing sigmas, fill in an "empty" sigma array anyway
					if(is.null(sigmaDim))
						sigmaFinal <- array(dim = c(0,0,1), dimnames = list(NULL, NULL, "estimates"))
					else
						sigmaFinal <- array(FinalEstimates$SIGMA, dim = c(dim(FinalEstimates$SIGMA), 1),
					  	dimnames = c(dimnames(sigmaInitial), list("estimates")))
				}
				colnames(thetaFinal) <- colnames(thetaInitial)
				
				# extract minimization status.  If this is missing, use an empty character vector so
				# that the slot type is correct
				minInfo <- unlist(attr(reportContents$Iter, "min.info"))
				if(is.null(minInfo)) minInfo <- character(0)
				if(is.null(outTables))
					outTables<- data.frame()
				# create the object
				new("NMBasicModel", parameterIterations = reportContents$Iter, 
						problemStatement = controlStatements$Prob,
						objectiveFinal = Objective.Minimum, 
						parameterCovMatrix = covMatrix,
						parameterCorMatrix = corMatrix,
						thetaInitial = thetaInitial,
						sigmaInitial = sigmaInitial,
						omegaInitial = omegaInitial,					
						thetaFinal = thetaFinal,
						sigmaFinal = sigmaFinal, omegaFinal = omegaFinal,			
						additionalVars = as.data.frame(matrix(ncol = 0, nrow = max( nOutDataRows, nInDataRows ))),
						inputData = inData, 
						outputData = outTables, 
						controlStatements = controlStatements,
						reportStatements = reportContents,
						minInfo = minInfo,
						nmVersionMajor = versionInfo["major"],
						nmVersionMinor = as.numeric(versionInfo["minor"])) 
						
			} ) # end with(reportContents)
	
}
