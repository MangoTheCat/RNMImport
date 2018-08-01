
#' Forces the data in a NONMEM object to conform to the variable formats specified in the metadata. Users will be allowed to choose a subset of 
#' variables which should be formatted.  Variables that are formatted should be coerced to factors, with the levels set as described by the comma 
#' separated string stored within the metadata.  
#' @param obj An object of class NMRun, NMBasicModel or data.frame
#' @param varSubset The variables to be converted to factors. It can be a vector of variable names or a comma separated string
#' @param ... Additional arguments that apply to different classes. These are \code{problemNum} which specifies the required run 
#'   		  for a NMRun object
#' @return A new object with the required data coerced to factors of the correct form
#' @examples
#' x <- data.frame(SMOK = sample(0:1, 10, replace = TRUE), SEX = sample(1:2, 10, replace = TRUE))
#' print(imposeCategoryFormat(x)) 
#' @author Mango Solutions

imposeCategoryFormat <- function(obj, varSubset, ...)
{
	NULL
}
setGeneric("imposeCategoryFormat")

imposeCategoryFormat.NMBasicModel <- function(obj, varSubset)
{
	if(missing(varSubset))
	{
		RNMImportWarning(msg = "No subset provided. All variables will be coerced to factor types.\n")
		varNames <- .RNMImportEnv$variables[["Variable"]][.RNMImportEnv$variables[["Format"]] != ""]
		formats <- .RNMImportEnv$variables[["Format"]][.RNMImportEnv$variables[["Format"]] != ""]
	}
	else
	{
		varNames <- CSLtoVector(varSubset)	
		formats <- .RNMImportEnv$variables[["Format"]][match(varNames, .RNMImportEnv$variables[["Variable"]])]
		formats <- formats[formats != ""]
		varNames <- varNames[formats != ""]
	}
	formatsList <- strsplit(formats, split = ",")
	names(formatsList) <- varNames
	#Create a data frame for each variable with the number against the description, e.g. SMOK would have 0 and 1 in one column, with smoker and non-smoker
	#in the other.
	matchList <- lapply(formatsList, function(x)
					{
						numLoc <- regexpr(pattern = "[0-9]", x)
						nums <- as.numeric(substring(x, numLoc, numLoc))
						equalLoc <- regexpr(pattern = "=\\s*", x)
						desc <- substring(x, equalLoc + attr(equalLoc, "match.length"))
						df <- data.frame(nums, desc, stringsAsFactors = FALSE)
						df[order(df$nums), ]
					})
	#Match against the data.frame above and force to category format.
	for(i in varNames)
	{	
		if(any(i %in% names(obj@inputData)))
		{
			obj@inputData[[i]] <- matchList[[i]]$desc[match(obj@inputData[[i]], matchList[[i]]$nums)]
			obj@inputData[[i]][is.na(obj@inputData[[i]])] <- matchList[[i]]$desc[1]
			obj@inputData[[i]] <- factor(obj@inputData[[i]])
			obj@outputData[[i]] <- matchList[[i]]$desc[match(obj@outputData[[i]], matchList[[i]]$nums)]
			obj@outputData[[i]][is.na(obj@outputData[[i]])] <- matchList[[i]]$desc[1]
			obj@outputData[[i]] <- factor(obj@outputData[[i]])
		}	
	}			
	invisible(obj)
}
setMethod("imposeCategoryFormat", signature(obj = "NMBasicModel"), imposeCategoryFormat.NMBasicModel)
setMethod("imposeCategoryFormat", signature(obj = "NMBasicModelNM7"), imposeCategoryFormat.NMBasicModel)

imposeCategoryFormat.NMRun <- function(obj, varSubset, problemNum = 1)
{
	specObj <- getProblem(obj, problemNum)
	newObj <- imposeCategoryFormat(specObj, varSubset)
	invisible(newObj)	
}
setMethod("imposeCategoryFormat", signature(obj = "NMRun"), imposeCategoryFormat.NMRun)

setOldClass("data.frame")
imposeCategoryFormat.data.frame <- function(obj, varSubset)
{
	if(missing(varSubset))
	{
		RNMImportWarning(msg = "No subset provided. All variables will be coerced to factor types.\n")
		varNames <- .RNMImportEnv$variables[["Variable"]][.RNMImportEnv$variables[["Format"]] != ""]
		formats <- .RNMImportEnv$variables[["Format"]][.RNMImportEnv$variables[["Format"]] != ""]
	}
	else
	{
		varNames <- CSLtoVector(varSubset)	
		formats <- .RNMImportEnv$variables[["Format"]][match(varNames, .RNMImportEnv$variables[["Variable"]])]
		formats <- formats[formats != ""]
		varNames <- varNames[formats != ""]
	}
	formatsList <- strsplit(formats, split = ",")
	names(formatsList) <- varNames
	#Create a data frame for each variable with the number against the description, e.g. SMOK would have 0 and 1 in one column, with smoker and non-smoker
	#in the other.
	matchList <- lapply(formatsList, function(x)
			{
				numLoc <- regexpr(pattern = "[0-9]", x)
				nums <- as.numeric(substring(x, numLoc, numLoc))
				equalLoc <- regexpr(pattern = "=\\s*", x)
				desc <- substring(x, equalLoc + attr(equalLoc, "match.length"))
				df <- data.frame(nums, desc, stringsAsFactors = FALSE)
				df[order(df$nums), ]
			})
	#Match against the data.frame above and force to category format.
	for(i in varNames)
	{	
		if(any(i %in% names(obj)))
		{
			obj[[i]] <- matchList[[i]]$desc[match(obj[[i]], matchList[[i]]$nums)]
			obj[[i]][is.na(obj[[i]])] <- matchList[[i]]$desc[1]
			obj[[i]] <- factor(obj[[i]])
		}	
	}			
	invisible(obj)
}
setMethod("imposeCategoryFormat", signature(obj = "data.frame"), imposeCategoryFormat.data.frame)

imposeCategoryFormat.NMSimModel <- function(obj, varSubset)
{
	if(missing(varSubset))
	{
		RNMImportWarning(msg = "No subset provided. All variables will be coerced to factor types.\n")
		varNames <- .RNMImportEnv$variables[["Variable"]][.RNMImportEnv$variables[["Format"]] != ""]
		formats <- .RNMImportEnv$variables[["Format"]][.RNMImportEnv$variables[["Format"]] != ""]
	}
	else
	{
		varNames <- CSLtoVector(varSubset)	
		formats <- .RNMImportEnv$variables[["Format"]][match(varNames, .RNMImportEnv$variables[["Variable"]])]
		formats <- formats[formats != ""]
		varNames <- varNames[formats != ""]
	}
	formatsList <- strsplit(formats, split = ",")
	names(formatsList) <- varNames
	#Create a data frame for each variable with the number against the description, e.g. SMOK would have 0 and 1 in one column, with smoker and non-smoker
	#in the other.
	matchList <- lapply(formatsList, function(x)
			{
				numLoc <- regexpr(pattern = "[0-9]", x)
				nums <- as.numeric(substring(x, numLoc, numLoc))
				equalLoc <- regexpr(pattern = "=\\s*", x)
				desc <- substring(x, equalLoc + attr(equalLoc, "match.length"))
				df <- data.frame(nums, desc, stringsAsFactors = FALSE)
				df[order(df$nums), ]
			})
	#Match against the data.frame above and force to category format.
	for(i in varNames)
	{	
		if(any(i %in%names(obj@inputData)))
		{
			obj@inputData[[i]] <- matchList[[i]]$desc[match(obj@inputData[[i]], matchList[[i]]$nums)]
			obj@inputData[[i]][is.na(obj@inputData[[i]])] <- matchList[[i]]$desc[1]
			obj@inputData[[i]] <- factor(obj@inputData[[i]])
			obj@outputData[[i]] <- matchList[[i]]$desc[match(obj@outputData[[i]], matchList[[i]]$nums)]
			obj@outputData[[i]][is.na(obj@outputData[[i]])] <- matchList[[i]]$desc[1]
			obj@outputData[[i]] <- factor(obj@outputData[[i]])
		}	
	}			
	invisible(obj)
}
setMethod("imposeCategoryFormat", signature(obj = "NMSimModel"), imposeCategoryFormat.NMSimModel)
setMethod("imposeCategoryFormat", signature(obj = "NMSimModelNM7"), imposeCategoryFormat.NMSimModel)

imposeCategoryFormat.NMSimDataGen <- function(obj, varSubset)
{
	if(missing(varSubset))
	{
		RNMImportWarning(msg = "No subset provided. All variables will be coerced to factor types.\n")
		varNames <- .RNMImportEnv$variables[["Variable"]][.RNMImportEnv$variables[["Format"]] != ""]
		formats <- .RNMImportEnv$variables[["Format"]][.RNMImportEnv$variables[["Format"]] != ""]
	}
	else
	{
		varNames <- CSLtoVector(varSubset)	
		formats <- .RNMImportEnv$variables[["Format"]][match(varNames, .RNMImportEnv$variables[["Variable"]])]
		formats <- formats[formats != ""]
		varNames <- varNames[formats != ""]
	}
	formatsList <- strsplit(formats, split = ",")
	names(formatsList) <- varNames
	#Create a data frame for each variable with the number against the description, e.g. SMOK would have 0 and 1 in one column, with smoker and non-smoker
	#in the other.
	matchList <- lapply(formatsList, function(x)
			{
				numLoc <- regexpr(pattern = "[0-9]", x)
				nums <- as.numeric(substring(x, numLoc, numLoc))
				equalLoc <- regexpr(pattern = "=\\s*", x)
				desc <- substring(x, equalLoc + attr(equalLoc, "match.length"))
				df <- data.frame(nums, desc, stringsAsFactors = FALSE)
				df[order(df$nums), ]
			})
	#Match against the data.frame above and force to category format.
	for(i in varNames)
	{	
		if(any(i %in% names(obj@inputData)))
		{
			obj@inputData[[i]] <- matchList[[i]]$desc[match(obj@inputData[[i]], matchList[[i]]$nums)]
			obj@inputData[[i]][is.na(obj@inputData[[i]])] <- matchList[[i]]$desc[1]
			obj@inputData[[i]] <- factor(obj@inputData[[i]])
			obj@outputData[[i]] <- matchList[[i]]$desc[match(obj@outputData[[i]], matchList[[i]]$nums)]
			obj@outputData[[i]][is.na(obj@outputData[[i]])] <- matchList[[i]]$desc[1]
			obj@outputData[[i]] <- factor(obj@outputData[[i]])
		}	
	}			
	invisible(obj)
}
setMethod("imposeCategoryFormat", signature(obj = "NMSimDataGen"), imposeCategoryFormat.NMSimDataGen)