
# The global environment holding metadata for RNMImport
.RNMImportEnv <- new.env()

# The default
DEFAULTLOGS <- c("stdReport", "detailedReport", "warnings", "highLevelParse", "lowLevelParse")

# The file extensions
FILEEXTENSIONS <- list(control = c("mod", "ctl", "con"), report = c("lst", "out", "sep"), 
					outputTable = c("fit", "tab"), inputData = c("", "txt", "dat"))



initializeLogs <- function()
{
	# create the list of logs
	.RNMImportEnv$logConnections <- vector(mode = "list", length = length(DEFAULTLOGS))
	names(.RNMImportEnv$logConnections) <- DEFAULTLOGS
	for(i in seq_along(DEFAULTLOGS))
		.RNMImportEnv$logConnections[[i]] <- NA
	# only this log is not NA by default, and it will be set to stdout
	.RNMImportEnv$logConnections[["stdReport"]] <- stdout()
}


initializeVariables <- function(libName = file.path(system.file(), "../"))
{
	
	fileName <- file.path(libName, "RNMImport", "configdata/NONMEM2_Variables.csv" )
	cat("Full path to configuration file: \n", fileName, "\n")
	defaultConfig <- try(read.table(file = fileName, header = TRUE, sep = ",", stringsAsFactors = FALSE), silent = TRUE)
	if(inherits(defaultConfig, "try-error"))
	{
		RNMImportWarning("Could not import NONMEM variable configuration file.\n")
	}else
	{
		.RNMImportEnv$variables <- defaultConfig
	}
}


initializeFileNameExtensions <- function()
{
	.RNMImportEnv$fileExtensions <- FILEEXTENSIONS
}

initializeDataPath <- function()
{
	.RNMImportEnv$dataPath <- vector(mode = "character", length = 0)
}

initializeSubsets <- function()
{
	.RNMImportEnv$subsets <- list("default" = c("MDV != 1", "EVID == 0", "AMT <= 0"), applyOnLoad = TRUE)
}

initializeMiscOptions <- function(libName = file.path(system.file(), "../"))
{
	.RNMImportEnv$unitTestPath <- file.path(libName, "RNMImport/unittests") 
}

.onLoad <- function(libname, pkgname)
{
	initializeLogs()
	initializeVariables(libname)
	initializeFileNameExtensions()
	initializeDataPath()
	initializeSubsets()
	initializeMiscOptions()

}

