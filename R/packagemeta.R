#' Gets a variable name and its associated characteristics.
#' @title Get a variable
#' @param varName A vector of variable names
#' @return A data frame with the label, format and type of each variable name
#' @author Mango Solutions
#' @export

getVarDescription <- function(varName)
{
#	if(!(all(varName %in% .RNMImportEnv$variables[["Variable"]])))
#		RNMImportStop(msg = "Variable name not in original configuration.\n")
	varName <- intersect(varName,.RNMImportEnv$variables[["Variable"]] )
	.RNMImportEnv$variables[.RNMImportEnv$variables[["Variable"]] %in% varName, ]
}

#' Sets new characteristics for a required variable.
#' @title Set new variable
#' @param varName The variable name
#' @param varLabel A new label for the variable. Default is NULL
#' @param varFormat A new format for the variable. Default is NULL
#' @param varType A new type for the variable. Default is NULL
#' @return None
#' @author Mango Solutions
#' @export
setVarDescription <- function(varName, varLabel = NULL, varFormat = NULL, varType = NULL)
{
	if(is.null(varLabel) && is.null(varFormat) && is.null(varType))
		RNMImportStop(msg = "You have not specified any changes to the original configuration.\n")
	if(!is.null(varLabel))
		.RNMImportEnv$variables[["Label"]][.RNMImportEnv$variables[["Variable"]] == varName] <- varLabel
	if(!is.null(varFormat))
		.RNMImportEnv$variables[["Format"]][.RNMImportEnv$variables[["Variable"]] == varName] <- varFormat
	if(!is.null(varType))
		.RNMImportEnv$variables[["VarType"]][.RNMImportEnv$variables[["Variable"]] == varName] <- varType
}

#' Adds a new variable and corresponding characteristics to a global data frame.
#' @title Add new variable
#' @param varName The variable name		
#' @param varLabel The variable label		
#' @param varType The variable type	
#' @param varFormat The variable format. Default is NULL
#' @return None
#' @author Mango Solutions
#' @export
addVarDescription <- function(varName, varLabel, varType, varFormat = NULL)
{
	if(varName %in% .RNMImportEnv$variables[["Variable"]])
		RNMImportStop(msg = "Variable name currently exists.\n")
	newRow <- c(varName, varLabel, ifelse(is.null(varFormat), "", varFormat), varType)
	.RNMImportEnv$variables <- rbind(.RNMImportEnv$variables, newRow)
	.RNMImportEnv$variables <- .RNMImportEnv$variables[order(.RNMImportEnv$variables[["Variable"]]), ]
}


#' Gets a path stored globally by the user.
#' @title Get user path
#' @param pathName The path name
#' @return The path
#' @author Mango Solutions
#' @export

getNmPath <- function(pathName)
{
	if(!(pathName %in% names(.RNMImportEnv$dataPath)))
		RNMImportStop(msg = "Path name does not exist!.\n")
	return(.RNMImportEnv$dataPath[[pathName]])
}

#' Sets a new path globally to be used by the user. 
#' @title Set new path
#' @param pathName The path name
#' @param path The path
#' @return None
#' @author Mango Solutions
#' @export
setNmPath <- function(pathName, path)
{
	.RNMImportEnv$dataPath[[pathName]] <- path
}

#' Removes a globally stored user path. 
#' @title Remove user path
#' @param pathName The path name
#' @return None
#' @author Mango Solutions
#' @export
removeNmPath <- function(pathName)
{
	numErase <- (1:length(.RNMImportEnv$dataPath))[pathName == names(.RNMImportEnv$dataPath)]
	.RNMImportEnv$dataPath <- .RNMImportEnv$dataPath[-numErase]
	if(!(length(.RNMImportEnv$dataPath)))
		attributes(.RNMImportEnv$dataPath) <- NULL
}



#' Retrieves the global default subset which is assigned to objects upon loading
#' @return Character vector of the default subset expressions
#' @author Mango Solutions
#' @export

defaultDataSubset <- function()
{
	return(.RNMImportEnv$subsets$default)
}

#' Modifies the global default subset for loaded objects, and can toggle if it is applied on loading
#' @param sub character vector of strings of subset expressions
#' @param applyOnLoad Logical flag.  Should the subset actually be applied on loading automatically?
#' @return None
#' @author Mango Solutions
#' @export

setDefaultDataSubset <- function(sub, applyOnLoad)
{
	assertClass(sub, "character")
	assertClass(applyOnLoad, "logical")
	# if it is a vector of length longer than one, take only the first element
	applyOnLoad <- applyOnLoad[1]
	.RNMImportEnv$subsets$default <- sub
	.RNMImportEnv$subsets$applyOnLoad <- applyOnLoad
}

#' Augments the global default subset by appending the "sub" parameter to the existing subset
#' @param sub  
#' @return None
#' @author Mango Solutions
#' @export

augmentDefaultDataSubset <- function(sub)
{
	assertClass(sub, "character")
	newSubset <- unique(c(defaultDataSubset(), sub))
	setDefaultDataSubset(newSubset, applySubsetOnLoad())
	
}

#' Retrieves the setting as to whether or not a subset should be attached to an object on loading, 
#' or can actually be used to toggle the setting
#' @param applyOnLoad If supplied, used to toggle the setting
#' @return Setting if argument is not supplied
#' @author Mango Solutions

applySubsetOnLoad <- function(applyOnLoad)
{
	if(missing(applyOnLoad))
		return( .RNMImportEnv$subsets$applyOnLoad )
	else
	{
		assertClass(applyOnLoad, "logical")
		.RNMImportEnv$subsets$applyOnLoad <- applyOnLoad[1]
	}
}
