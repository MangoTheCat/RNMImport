
#' Imports the input data used in an individual basic NONMEM model based on its $INPUT
#' and $DATA statements
#' @title Import NONMEM input data
#' @param dataStatement [char matrix] - A data statement, as parsed by .importNmModData
#' @param inputStatement [char matrix] - A 2 column matrix describing the $INPUT statement, see .importNmModInput
#' @param dropCols [L,1].  If TRUE, columns  with XXXX=DROP are eliminated from returned data
#' @param trim Currently unused
#' @param path path where the data files are located
#' @return  A data.frame with the data inside the file
#' @author Mango Solutions

importModelData <- function(
		dataStatement, inputStatement, dropCols = TRUE, trim=FALSE,	path = NULL, duplicateAliased = TRUE)
{	
	# TODO: need to handle the case where inputs are read from the previous problem's output 
	
	
    fileName <- dataStatement[,"File"]
	fileName <-  gsub('( IGNORE | IGN).*','', fileName, ignore.case = TRUE)
	stopifnot(!is.null(fileName))
		
	fileName <- .getFile(fileName, path = path)
	
	if( !.canOpen( fileName )) 
		RNMImportStop(paste("Unable to open requested data file ", fileName , "\n") )
	 
	# now extract the options from the data statement, converting FORTRAN 95 ops to 77 
	# as needed

	ignore <-dataStatement[,"IG"]
	accept <- dataStatement[,"ACCEPT"]
	translate <- dataStatement[,"TRANSLATE"]
	records <- dataStatement[,"RECORDS"]
	
	# split out the ignore statement, as individuals tokens (or chunks of code) are separated by ";"
	ignoreTokens <- unlist(strsplit(ignore, split = ";"))
	# replace "NONE" with "#"
	ignoreTokens <- ifelse(ignoreTokens == "NONE", "#", ignoreTokens)
	
	ignoreCodes <- ignoreTokens[which(nchar(ignoreTokens) > 1)]
	ignoreChars <- ignoreTokens[which(nchar(ignoreTokens) == 1)]
	if(length(ignoreChars) == 0) ignoreChars <- ""
	
	# Call readNmData, which is the workhorse function that actually reads the table
	# single IGNORE=c characters are handled here, whereas IGNORE=(list) is handled below 
	myData <- readNmData(file = fileName, ignore = ignoreChars, 
			translate = translate)
	if(inherits(myData,'try-error')){
		return(simpleError(myData))
	}	
	# Deal with the case of additional columns in the dataset
	if(nrow(inputStatement) == (ncol(myData) - 1) && all(is.na(myData[, ncol(myData)]))) 
		myData <- myData[,  - length(myData), drop = F]
	
	# determine aliased columns

	aliasedColumns <- (toupper(inputStatement[,1]) != "DROP" & toupper(inputStatement[,2]) != "DROP") & (toupper(inputStatement[,1]) != toupper(inputStatement[,2]))

	# if there are aliased columns, we will repeat them with their aliased name
	if(any(aliasedColumns))
	{
		# if there are more input columns than are named in the inputStatement, we pad the aliasedColumn vector
		aliasedColumnsSelect <- aliasedColumns
		if(ncol(myData) > length(aliasedColumns))
			aliasedColumnsSelect <- c(aliasedColumns, rep(FALSE, length = ncol(myData) - length(aliasedColumns)) )
		aliasColumnBlock <- myData[ , aliasedColumnsSelect, drop = FALSE]
		
		names(aliasColumnBlock) <- inputStatement[ aliasedColumns, 2]
	}
	# now determine which columns should be dropped in case dropCols = TRUE
	if(dropCols) 
	{
		colsToKeep <- toupper(inputStatement[, 1]) != "DROP" & toupper(inputStatement[, 2]) != "DROP"
		# if there are extra columns, need to extend the columns to keep
		colsToKeepSel <- colsToKeep
		if(length(colsToKeepSel) < ncol(myData))
			colsToKeepSel <- c(colsToKeep, rep(TRUE, ncol(myData) - length(colsToKeep) ))
	}
	else {
		colsToKeep <- rep(TRUE, nrow(inputStatement))
		colsToKeepSel <- rep(TRUE, ncol(myData))
	
	}
	myData <- myData[, colsToKeepSel, drop = FALSE]
	
	# save the number of input columns before any alias duplication is performed
	
	numInDataColumns <- ncol(myData)
	
	# Calculate columns names
	
	cNames <- ifelse(toupper(inputStatement[, 1]) == "DROP", inputStatement[, 2], inputStatement[, 1])[colsToKeep]

	nDiff <- numInDataColumns - length(cNames)
	# handle columns not present in the $INPUT statement but present in the data file
	if(nDiff != 0)
		RNMImportWarning(paste("\nWarning: Number of columns in datafile (", ncol(myData), ") does not equal number of columns in $INPUT statement (", length(cNames), ")\n", sep = ""))
	if(nDiff > 0)
		cNames <- c(cNames, paste("ExtraCol", 1:nDiff, sep = ""))
	if(nDiff < 0)
		cNames <- cNames[1:ncol(myData)]
	dimnames(myData) <- list(dimnames(myData)[[1]], cNames)
	
	# now add aliased data
	if(any(aliasedColumns))
		myData <- cbind(myData, aliasColumnBlock)
	
	# apply the IGNORE=(list) statement
	
	for(ignoreCode in ignoreCodes)
	{
		# convert "=" to ".EQ."
		ignoreCode <- gsub(ignoreCode, pattern = "[[:space:]]*=[[:space:]]*", replacement = ".EQ.")
		.readNmData.nmSubset(data = myData, nmCode = ignoreCode, method = "ignore", na.keep = TRUE)
	}
	# handle ACCEPT=(list)
	if(accept != "")
		.readNmData.nmSubset(data = myData, nmCode = accept, method = "accept", na.keep = TRUE)
	
	# handle RECORDS=
	# note that according to the NONMEM manual, this should come last
	if(records != "")
		.readNmData.nmRecords( records, data = myData )
	
	### Check numeric and missing values
	myData <- .importDataNumeric(myData, missToZero = FALSE)
	
	### Use .deriveNmColumns
	if(!trim)
		myData <- .deriveNmColumns(myData)
	
	myData
	
}
