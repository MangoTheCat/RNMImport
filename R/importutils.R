
 
#' Import table files "safely".  This routine attempts to import the Table element of a control file, but does
#' so with additional error handling.  In particular, if the table statement is NULL, or all tables are missing,
#' an empty data.frame is returned, rather than an exception
#' @param tableStatement The Table element of an nmModel class object 
#' @param path Path to the table files
#' @title Import table files safely
#' @return The value returned by importModelOutputTables
#' @author Mango Solutions

.importTablesSafely <- function(tableStatement = NULL, path = "")
{
	# NULL table statement should lead to empty data.frame
	if(is.null(tableStatement))
		outTables <- data.frame()
	else {
                # try to import, and if one can't, catch the error and return an empty data.frame
                lookFor <- names(tableStatement)[min(grep('^File*', names(tableStatement)))]
                missingFiles <- which(!file.exists(file.path(path, tableStatement[,lookFor])))
                if(length(missingFiles)>0){
                    msg <- 
                            paste('cannot find', paste(tableStatement[missingFiles,lookFor], collapse=','))
                    RNMImportWarning(msg)
                }

		# try to import, and if one can't, catch the error and return an empty data.frame
		outTables <- try(importModelOutputTables( tableStatement , path = path )) 
		if( inherits( outTables, "try-error" ) )
		{
			RNMImportWarning("Unable to import table files, will use empty output data set \n")
			outTables <- data.frame()
		}
	}
	outTables
}

#' Reads the lines of a file and removes whitespace in the process
#' @param file name of the file to read
#' @param empty.rx regular expression to eliminate from strings read in 
#' @title Read file contents and strip whitespace
#' @return Character vectors of lines of file contents
#' @author Mango Solutions
#' @keywords IO

# Original author: R. Francois

scanFile <- function( file, empty.rx = "^[[:space:]]*$" )
{	
	# check if the file is NULL or can't be opened
	if( is.null(file) || !.canOpen(file) ) 
	{
		RNMImportWarning(paste(file, " is not valid, returning NULL"))	
		return(NULL)
	}
	contents <- scan( file, sep = "\n", what = "character", 
			blank.lines.skip = TRUE, multi.line = FALSE, quiet = TRUE)
	# remove whitespace in the file content and return
	contents <- negGrep( empty.rx , contents, value = TRUE )
	if( length(contents) > 0) contents else NULL
}

#' Checks whether or not a file can be opened
#' @param path.to.file Full path to the file
#' @title Check if file can be opened
#' @return logical 
#' @note Originally by John James
#' @author Mango Solutions
#' @keywords IO

.canOpen <- function(path.to.file)
{
	
	if(!file.exists(path.to.file))
		return(FALSE)
	info <- file.info(path.to.file)
	
	### directory
	if(info[1,2])
		return(FALSE)
	
	### if zero size assume NOK
	if(info[1,1]==0)
		return(FALSE)
	TRUE
}

#' Retrieves the full path containing some file specified in a relative
#' path. Simply wraps file_path_as_absolute
#' @param x Path to the file 
#' @return String containing absolute path
#' @author Mango Solutions

dirname.abs <- function(x){
	dirname( tools::file_path_as_absolute(x) )
}

#' Checks whether or not a string is a full path name
#' @param x String to check
#' @title Check if string is full path
#' @return logical, TRUE if x is a path
#' @author Mango Solutions
#' @keywords utils

.isfullpath <- function( x ){
	# x %~% "^(/|\\\\|[[:alpha:]]:)"
	regexMatches(x, "^(/|\\\\|[[:alpha:]]:)" )
}

# ??
#' @param file 
#' @param path 
#' @title isinpath
#' @return logic
#' @author Mango Solutions
#' 

.isinpath <- function( file, path){
	nchar( file ) > nchar( path ) && substring( file, 1, nchar(path) ) == path
}

#' Takes a file name, a path, and correctly appends them to form a full relative path to the file
#' @param file Filename (possibly including some path data) 
#' @param path A path
#' @param test (?)
#' @param warn (?)
#' @param stop (?)
#' @param remove (?)
#' @return A full relative path containing the file name
#' @author Mango Solutions


.getFile <- function( 
		file, 
		path = NULL,
		test = FALSE,
		warn = TRUE, 
		stop = FALSE, 
		remove = TRUE
){
	file <- as.character( file )
	# if file has commas, take it to be a comma-seperated list
	if(any(regexMatches(file, ",")))
		file <- CSLtoVector(file)
	# RNM-206: If the file(s) are enclosed in double quotes (e.g. to allow a $DATA file to have a space in its name) then strip these off
	file <- gsub("^\"(.*)\"$", "\\1", file)
	
	## rx to identify if this is a full path file
	if( !is.null(path) ) {
		for( i in seq(along=file)){
			file[i] <- if(.isfullpath(file[i]) || .isinpath(file[i], path)) file[i] else file.path( path, file[i])
		}
	}
	if(test) test <- file.exists( file )
	if( any(test) ){
		if(stop) RNMImportStop(paste("missing files", file[test], sep = "\n\t\t"))
		if(warn) logMessage(paste("Warning: Cannot open file", file[test]), logName= "warnings")
		if(remove) file <- file[!test]
	}
	if( !length(file) ) file <- NULL
	file
}

#' "Processes" a path, so that if it is enclosed in brackets, it is taken to be the name of a path
#' to be retrieved by "getNmPath".  Otherwise, just returns the path
#' @param path the path to process.
#' @return Either path unmodified, or path retrieved via getNmPath
#' @author Mango Solutions

processPath <- function(path)
{
	path <- path[1]
	# path names should be enclosed in brackets surrounded by spaces
	PATHNAMEREGEX <- "^[[:space:]]*\\([[:alnum:]]+\\)[[:space:]]*$"
	if(any(regexMatches(path, PATHNAMEREGEX)))
		return(getNmPath( bracketPop(path, inPlace = FALSE)$op.out ))
	path
	
}

#' Checks if a file has a given extentions
#' @param fileName Name of the file to check
#' @param extension Extension of the file to check.  Can have more than one
#' @return whether or not the filename has the given extension 
#' @author Mango Solutions

hasExtension <- function(fileName, extensions)
{
	y <- sapply(casefold(extensions), 
			function(x) { regexMatches(txt = casefold(fileName), rx = paste( "\\.", x, "$", sep = "")) } )
	any(y)
}


#' Changes a filename to lower case if the current system is Windows, otherwise leaves it unchanged.
#' @param fileName  
#' @return fileName in lower case, 
#' @author Mango Solutions


.windowsToLower <- function(fileName)
{
	if(Sys.info()["sysname"] == "Windows")
		return(tolower(fileName))
	fileName
}

#' @param contStates 
#' @param pri 
#' @return does the string, basically PRIOR, exist in the control file?
#' @author Mango Solutions
.lookFor <- function(contStates=names(obj@controlStatements), subr=NULL, pri='^ +[$](PRIOR|PRI)|PRIOR='){
	test2 <- FALSE
	test1 <- any(regexpr(pri, contStates, ignore.case=TRUE)>0)
	if(length(subr)>0){
		test2 <- any(regexpr(pri, subr, ignore.case=TRUE)>0)
	}
	test1 || test2
}
