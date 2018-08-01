

#' Displays information about output data.  Utility function for use with show methods
#' @param object NMProblem object 
#' @title Show output table info
#' @return none
#' @author Mango Solutions

showOutput <- function(object)
{
	
	controlStatements <- object@controlStatements
	with( controlStatements, 
		{ 
			# output data will be a list if there were "FIRSTONLY" tables
			if(class(object@outputData) != "list")
			{
				if(all(dim(object@outputData) == c(0,0)))
					cat("No output table data\n")
				else
				{
					cat("Output table files: ")
					cat(paste(Tables[,"File"], collapse = ","), "\n")
					cat("Output table dimensions:\n")
					cat(dim(object@outputData), "\n")
					cat("Output table columns:\n")
					cat(colnames(object@outputData), "\n")
				}	
			}
			else
			{
				cat("Output table files: ")
				cat(paste(Tables[,"File"], collapse = ","), "\n")
				cat("Output table dimensions:\n")
				
				if(!is.null(object@outputData[["normal.tables"]]))
					cat("Normal tables: ",  dim(object@outputData[["normal.tables"]]), "\n")
				if(!is.null(object@outputData[["firstonly.tables"]]))
					cat("FIRSTONLY tables: ",  dim(object@outputData[["firstonly.tables"]]), "\n")
				
				cat("Output table columns:\n")
				if(!is.null(object@outputData[["normal.tables"]]))
					cat("Normal tables: ", colnames(object@outputData[["normal.tables"]]), "\n")
				if(!is.null(object@outputData[["firstonly.tables"]]))
					cat("FIRSTONLY tables: " , colnames(object@outputData[["firstonly.tables"]]), "\n")
				
			}
	})
}

# TODO: reduce amount of copy-paste code in this!

show.NMRun <- function(object)
{
	fileInfo <- getFileinfo(object)
	cat("Control file: \n")
	print(fileInfo["controlFile",])
	cat("Output report file: \n")
	print(fileInfo["reportFile", ])
	cat("Number of problems:", object@numProblems, "\n")
	cat("Problems:\n******************* \n")
	for(i  in seq_along(object@problems))
	{
		cat("Problem ", i, "\n\n")
		show(object@problems[[i]])
	}
}

setMethod("show", signature(object = "NMRun"), show.NMRun )

show.NMBasicModel <- function(object)
{
	controlStatements <- object@controlStatements
	
	cat("Standard NONMEM problem:\n###############\n")
	
	with( controlStatements, 
			{
				
				cat("Problem statement: ", Problem, "\n")
				
				cat("Data file:", Data[,"File"], "\n")
				
				
					cat("Input table dimensions:\n")
					cat(dim(object@inputData), "\n")
					cat("Input table columns:\n")
					cat(colnames(object@inputData), "\n")
				
				if(!is.null(controlStatements$PRED))
				{		
					cat("PRED:\n")
					print(PRED)
					cat("\n")
				}
				if(!is.null(controlStatements$PK)) 
				{
					cat("PK: \n")
					print(PK)
					cat("\n")
				}
				if(!is.null(controlStatements$Error))
				{
					cat("Error:\n")
					print(Error)
				}
				cat("Parameter estimates:\n###############\n")
				cat("THETAs:\n")
				print(object@thetaFinal["estimates",])
				cat("OMEGAs:\n")
				print(object@omegaFinal[,,"estimates"])
				cat("SIGMAs:\n")
				print(object@sigmaFinal[,,"estimates"])
				
				showOutput(object)
			} ) # end with(controlStatements ...
}

setMethod("show", signature(object = "NMBasicModel"), show.NMBasicModel)

show.NMBasicModelNM7 <- function(object)
{
	controlStatements <- object@controlStatements
	
	cat("Standard NONMEM (7) problem:\n###############\n")
	
	with( controlStatements, 
			{
				
				cat("Problem statement: ", Problem, "\n")
				
				cat("Data file:", Data[,"File"], "\n")
				
				cat("Input table dimensions:\n")
				cat(dim(object@inputData), "\n")
				cat("Input table columns:\n")
				cat(colnames(object@inputData), "\n")
				
				if(!is.null(controlStatements$PRED))
				{		
					cat("PRED:\n")
					print(PRED)
					cat("\n")
				}
				if(!is.null(controlStatements$PK)) 
				{
					cat("PK: \n")
					print(PK)
					cat("\n")
				}
				if(!is.null(controlStatements$Error))
				{
					cat("Error:\n")
					print(Error)
				}
				cat("Methods used: \n")
				print(object@methodNames)
				cat("Parameter estimates:\n###############\n")
				cat("THETAs:\n")
				print(object@thetaFinal)
				cat("OMEGAs:\n")
				print(object@omegaFinal)
				cat("SIGMAs:\n")
				print(object@sigmaFinal)
				
				showOutput(object)
			} ) # end with(controlStatements ...
}

setMethod("show", signature(object = "NMBasicModelNM7"), show.NMBasicModelNM7)


show.NMSimDataGen <- function(object)
{
	controlStatements <- object@controlStatements	
	
	with(controlStatements, 
		{
			cat("NONMEM data simulation problem without model fitting step:\n###############\n")
			cat("Problem statement: ", Problem, "\n")
			
			cat("Data file:", Data[,"File"], "\n")
			cat("Input table dimensions:\n")
			cat(dim(object@inputData), "\n")
			cat("Input table columns:\n")
			cat(colnames(object@inputData), "\n")
			
			showOutput(object)
			
			cat("Number of simulations performed: ", Sim["nSub"], "\n")
			
			cat("Seeds: ", na.omit(object@seeds), "\n" )
			cat("Initial value of paremeters:\n")
			cat("THETAs:\n")
			print(object@thetaInitial)
			cat("OMEGAs:\n")
			print(object@omegaInitial)
			cat("SIGMAs:\n")
			print(object@sigmaInitial)
				
		} ) # end with(controlStatments, 
}

setMethod("show", signature(object = "NMSimDataGen"), show.NMSimDataGen)

show.NMSimModel <- function(object)
{
	controlStatements <- object@controlStatements
	with(controlStatements, 
	{
		
		cat("NONMEM data simulation problem:\n###############\n")
		cat("Problem statement: ", Problem, "\n")
		if(!is.null(controlStatements$PRED))
		{		
			cat("PRED:\n")
			print(PRED)
			cat("\n")
		}
		if(!is.null(controlStatements$PK)) 
		{
			cat("PK: \n")
			print(PK)
			cat("\n")
		}
		cat("Data file:", Data[,"File"], "\n")
		cat("Input table dimensions:\n")
		cat(dim(object@inputData), "\n")
		cat("Input table columns:\n")
		cat(colnames(object@inputData), "\n")
		
		showOutput(object)
		
		cat("Number of simulations performed: ", Sim["nSub"], "\n")
		
		cat("Seeds: ", na.omit(object@seeds), "\n" )
		
		# now display estimates
		cat("Final estimates for each subproblem:\n\n")
		
		cat("THETAs:\n")
		print(object@thetaFinal)
		cat("OMEGAs:\n")
		print(object@omegaFinal)
		cat("SIGMAs:\n")
		print(object@sigmaFinal)
	
		cat("Objective function(s): \n")
		print(object@objectiveFinal)
	})
}

setMethod("show", signature(object = "NMSimModel"), show.NMSimModel)


show.NMSimModelNM7 <- function(object)
{
	controlStatements <- object@controlStatements
	with(controlStatements, 
			{
				
				cat("NONMEM (7) data simulation problem:\n###############\n")
				cat("Problem statement: ", Problem, "\n")
				if(!is.null(controlStatements$PRED))
				{		
					cat("PRED:\n")
					print(PRED)
					cat("\n")
				}
				if(!is.null(controlStatements$PK)) 
				{
					cat("PK: \n")
					print(PK)
					cat("\n")
				}
				cat("Data file:", Data[,"File"], "\n")
				cat("Input table dimensions:\n")
				cat(dim(object@inputData), "\n")
				cat("Input table columns:\n")
				cat(colnames(object@inputData), "\n")
				
				showOutput(object)
				
				cat("Number of simulations performed: ", Sim["nSub"], "\n")
				
				cat("Seeds: ", na.omit(object@seeds), "\n" )
				cat("Methods used: \n")
				print(object@methodNames)
				# now display estimates
				cat("Final estimates for each subproblem:\n\n")
				
				cat("THETAs:\n")
				print(object@thetaFinal)
				cat("OMEGAs:\n")
				print(object@omegaFinal)
				cat("SIGMAs:\n")
				print(object@sigmaFinal)
				
				cat("Objective function(s): \n")
				print(object@objectiveFinal)
			})
}

setMethod("show", signature(object = "NMSimModelNM7"), show.NMSimModelNM7)