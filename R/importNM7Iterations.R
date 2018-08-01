

#' Imports a set of NONMEM 7 iterations based on NONMEM 7 control file directives.
#' These directives can be found next to each $EST specification (e.g. FILE=...).  A list of data.frames of iterations
#' will be returned. This routine is not meant to be used directly by an end user
#' 
#' @param files Vectors of names of iteraton (.ext) files
#' @param noTitles Character vector of 
#' @param noLabels A character vector (Should have same number of elements as \code{files}.  
#' Should be vector of "0" and "1" entries.  Currently, this routine will produce an error if "1" is used at all
#' as NOLABLES=1 is not handled yet 
#' @param path String with path in which the files are to be found
#' @title Import NONMEM7 parameter iterations from .EXT files
#' @return A list of data.frames of iteration information.  Each data.frame
#' will have been loaded via \code{importNmTable}
#' @author Mango Solutions
#' 

importNm7Iterations <- function( files = NULL, noTitles = NULL, noLabels = NULL, methods = NULL, path = "." )
{
	numFiles <- length(files)
	if(numFiles == 0) return(list())
	
	RNMImportStopifnot(all(c(length(noTitles) == numFiles, length(noLabels) == numFiles, 
					length(methods) == numFiles)), 
			msg = "Inconsistent length of vector parameters!", match.call())
	
	RNMImportStopifnot( all(noLabels == "0") 
	, msg = "NOLABELS=1 option currently disallowed", match.call())
	
	# if an iteration file name is duplicated, it should (apparently) be disregarded.  
	files[duplicated(files)] <- ""

	iterations <- vector(mode = "list", length = numFiles)
	for(i in seq_along(files))
	{
		if(files[i] == "") next
		if(!file.exists(file.path(path, files[i]))){
			RNMImportWarning(paste(files[i], 'does not exist'))
			next		
		}
		iterations[[i]] <- importNm7Tables(files[i], type = "ext", tableTitles = noTitles[i] == "0" , path = path)
		
		# list elements will be named after estimation methods
		
		names(iterations[[i]]) <- sapply(iterations[[i]], function(x) attr(x, "method"))
	}
	
	do.call(c, iterations)
}
