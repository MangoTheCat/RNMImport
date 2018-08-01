na.omit.NMProblem <- function(prob, dataTypes = c("input", "output", "added"))
{
	dataTypes <- intersect(dataTypes, c("input", "output", "added"))
	# RNMImportWarning("Removing missing values may cause problems due to row number incompatibilities\n")
	result <- prob
	if("input" %in% dataTypes)
		result@inputData <- na.omit(result@inputData)
	if("output" %in% dataTypes)
		result@outputData <- na.omit(result@outputData)
	if("added" %in% dataTypes)
		result@additionalVars <- na.omit(result@additionalVars)
	
	result
}

na.replace.NMProblem <- function(prob, replacement = NA, dataTypes = c("input", "output"))
{
	dataTypes <- intersect(dataTypes, c("input", "output", "added"))
	result <- prob
	replacement <- rep(replacement, length.out = length(dataTypes))
	names(replacement) <- dataTypes
	if("input" %in% dataTypes)
		result@inputData[is.na(result@inputData)] <- replacement["input"]
	if("output" %in% dataTypes)
		result@outputData[is.na(result@outputData)] <- replacement["output"]
	if("added" %in% dataTypes)
		result@outputData[is.na(result@additionalVars)] <- replacement["added"]
	result
}