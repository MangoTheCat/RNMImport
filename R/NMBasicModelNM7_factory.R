

#' Constructs an NMBasicModel object from the control statements and output list statements that apply to it.
#' Meant to be used from within importNm
#' @title NMBasicModel constructor
#' @param controlStatements [list] A list of control file statements for this particular problem  
#' @param path [C,1] path parameter, passed directly to importModelData and importModelOutputTables
#' @param lstContents [list] contents of an lst file that apply to this problem
#' @param versionInfo [C, +] numeric vector that holds
#' @return An NMBasicModel object holding the problem information
#' @author Mango Solutions
#' @exportClass NMBasicModelNM7

NMBasicModelNM7 <- function(controlStatements, path, reportContents, dropInputColumns = FALSE, 
		versionInfo = c("major" = "VII", "minor" = 1), conFile=NULL)
{
	inData <- try(importModelData(dataStatement = controlStatements$Data,inputStatement = controlStatements$Input, path = path,
					dropCols = dropInputColumns), silent = TRUE)
	# if we could not read data file for some reason, continue anyway
	if(inherits(inData, "try-error"))
	{
		msg <- paste("Could not import data file.  Error generated was:",
				inData, "\nWill continue importing other components\n")
		inData <- data.frame()
	} # end if(inherits(inData, "try-error"))
	
	# import output tables if the $TABLE statement is present, else outdata is empty
	
	outTables <- .importTablesSafely(controlStatements$Table, path = path  )
	
	# need to know how many rows the data has, handle FIRSTONLY case here
	if(inherits(outTables, "list")) nOutDataRows <- max(sapply(outTables, nrow))
	else nOutDataRows <- nrow(outTables)
	nInDataRows <- nrow(inData)
	if(nInDataRows != nOutDataRows){
		msg <- paste("Number of rows of output data", nOutDataRows, 
				"\ndoes not match the number of rows of input data", nInDataRows,
				"!\n")
		cat(msg)
		RNMImportWarning(msg)
    }
	# automatically import NONMEM7-generated iterations from files if available:
	estStatement <- as.data.frame(controlStatements$Estimates)
    if (is.null(estStatement$method)) {
        RNMImportWarning('no METHOD found in EST statement, inferring FO!')
        estStatement$method <- 'FO'
    }
    if (all(nchar(estStatement$file)==0)) {
        if (is.null(conFile)) {
            conFile <- '.*'
        }else{
            conFile <- sub('\\.[^\\.]+$', '' , conFile)
        }
        extFn <- grep(sprintf('%s\\.ext$',conFile), list.files(path=path), ignore.case=TRUE, value=TRUE)
        if (length(extFn) < 1) {
            RNMImportWarning('no FILE specified in EST and no ext file found')
        } else {
            RNMImportWarning(sprintf('no FILE statement in EST, inferring [%s]!', extFn[1]))
        }
        estStatement$file <- extFn[1]
    }

    paramIter <- try( importNm7Iterations( files = estStatement[,"file"], noLabels = estStatement[,"noLabel"],
                    noTitles = estStatement[,"noTitle"], methods = estStatement[,"method"], path = path) , silent = TRUE )
    if(inherits(paramIter, "try-error")) { 
        RNMImportWarning("Unable to import parameter iterations, proceeding anyway\n")
        paramIter <- list()
    }
	
	with(reportContents,
			{
				# check for the covariance/correlation matrices
				covMatrices <- lapply(MethodResults, "[[", "CovarianceMatrix")
				corMatrices <- lapply(MethodResults, "[[", "CorrelationMatrix")
				
				# grab parameter initial values
				thetaInitial <- t(controlStatements$Theta[,c("Lower","Est","Upper")])
				
				# these may be missing in the control statements, so try to extract them from the reportContents
				omegaInitial <- if(!is.null(controlStatements$Omega)) controlStatements$Omega$initialMatrix  else  MethodResults[[1]]$initialEstimates$OMEGA
				
				# grab dimensions of omega final estimates
				omegaDim <- dim(MethodResults[[1]]$FinalEstimates$OMEGA)
				
				# if no initial omega, fall back on a default set of names
				
				if(is.null(omegaInitial)) {
					omegaInitial <- matrix(NA, nrow = omegaDim[1], ncol = omegaDim[2])
					omegaDimNames <- list(paste( "OMEGA", 1:omegaDim[1], sep = "" ), paste( "OMEGA", 1:omegaDim[2], sep = "" ))
				}
				
				else omegaDimNames <- dimnames(omegaInitial)
				
				sigmaInitial <- controlStatements$Sigma$initialMatrix
				if(is.null(sigmaInitial)) sigmaInitial <- matrix()
				rownames(thetaInitial) <- c("lowerBound", "initial", "upperBound")
				
				# get standard errors
				stdErrors <- lapply(MethodResults, "[[", "StandardError")
				
				omegaStdErrors <- lapply(stdErrors, "[[", "OMEGA")
				sigmaStdErrors <- lapply(stdErrors, "[[", "SIGMA")
				thetaStdErrors <- lapply(stdErrors, "[[", "THETA")
				
				# extract lists of final estimates by method
				
				thetaFinal <- lapply(MethodResults, function(x) x$FinalEstimates$THETA)
				omegaFinal <- lapply(MethodResults, function(x) x$FinalEstimates$OMEGA)
				sigmaFinal <- lapply(MethodResults, function(x) x$FinalEstimates$SIGMA)					
				
			#	colnames(thetaFinal) <- colnames(thetaInitial)
				
				objectiveFinal <- sapply(MethodResults, "[[", "Objective.Final")
				methodsUsed <- sapply(MethodResults, "[[", "method")
				ETAshrinks <- lapply(MethodResults, "[[", "ETAshrink") 
				EPSshrinks <-  lapply(MethodResults, "[[", "EPSshrink") 
				minInfo <- sapply(MethodResults, "[[", "TermStatus")
				# attr(objectiveFinal, "methods") <- methodsUsed
				# create the object
				new("NMBasicModelNM7", parameterIterations = paramIter, 
						problemStatement = controlStatements$Prob,
						objectiveFinal = objectiveFinal, 
						parameterCovMatrices = covMatrices,
						parameterCorMatrices = corMatrices,
						methodNames = methodsUsed,
						thetaInitial = thetaInitial,
						sigmaInitial = sigmaInitial,
						omegaInitial = omegaInitial,					
						
						thetaFinal = thetaFinal,
						sigmaFinal = sigmaFinal, omegaFinal = omegaFinal,
						
						sigmaStderr= sigmaStdErrors,
						omegaStderr = omegaStdErrors,
						thetaStderr = thetaStdErrors,
						
						ETAShrinkage = ETAshrinks,
						EPSShrinkage = EPSshrinks,
						
						additionalVars = as.data.frame(matrix(ncol = 0, nrow = max( nOutDataRows, nInDataRows ))),
						inputData = inData, 
						outputData = outTables,
						methodInfo =  controlStatements$Estimates ,
						controlStatements = controlStatements,
						reportStatements = reportContents,
						minInfo = minInfo,
						nmVersionMajor = versionInfo["major"],
						nmVersionMinor = as.numeric(versionInfo["minor"])) 
				
			} ) # end with(reportContents)
	
}

