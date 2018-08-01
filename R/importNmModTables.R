

#' Parses/ Imports the "TABLE" statement information from a NONMEM control file
#' @param txt [C,1] - Contents of a control file OR the contents of the $TABLE statement itself
#' @param .extract Logical flag.  Is this an actual
#' @param file Optional.  File from where txt originates
#' @title importNmModTables
#' @return A data.frame of descriptor information about each line in the $TABLE statement, including
#' which columns are to be written, whether there is a header or not, etc.
#' @author Mango Solutions
#' 

.importNmModTables <- function
(
		txt = NULL,
		.extract = !is.null(txt) && length(grep("\\$TAB", toupper(txt))) > 0,
		file = NULL
)
{
	logMessage(logName = "highLevelParse", "Entering .importNmModTables\n")
	### extract the TAB sections from the character vector
	tabSec <- if( .extract ) section( txt , "TAB", "", stripout = TRUE, glue = TRUE ) else txt
	if( is.null(tabSec) || length(tabSec) == 0 ) return( NULL )
	
	path <- if( !is.null(file) ) dirname.abs(file)
	
	# internal function to extract the information from each $TABLE statement line
	
	.extractTabInfo <- function(x)
	{
		# begin by cleaning the line of text
		x <- gsub("[[:space:]]+=", "=", x)
		x <- gsub("=[[:space:]]+", "=", x)
		x <- gsub("\\(", ".", x)
		x <- gsub("\\)", ".", x)
		
		# TODO: If FILE is missing, need to handle this gracefully, since this might be allowed
		fileName <- equalExpressionPop(x, "FILE", inPlace = TRUE,sep="=")
		RNMImportStopifnot(!is.null(fileName), match.call())

        ### handle the FORMAT=,1PE11.4 like statements
        format.of.table = equalExpressionPop(x, "FORMAT", inPlace=TRUE, sep = '=' )

		### handle the HEADER
		noHeader <- !ynPop( x, "HEADER", yes.prefix = "ONE", default = TRUE , inPlace = TRUE)
		
		### Populate the Columns
		# remove the nonmem keywords
		
		forward <- ynPop( x, "FORWARD", inPlace = TRUE )
		append <- ynPop( x, "APPEND", inPlace = TRUE)
		#firstOnly <- ynpop(x, "FIRSTONLY")
		
		# Now check for the presence of a "FIRSTONLY" statement
		
#		firstOnly <- length(grep(x, pattern = paste("[[:space:]]","FIRSTONLY","[[:space:]]", sep =""))) > 0
		firstOnly <- length(grep(x, pattern = paste("[[:space:]]?","FIRST|FIRSTONLY|FIRSTRECORDONLY|FIRSTRECONLY","[[:space:]]?", sep =""))) > 0
		
		nmKey <- c("noprint", "noprin", "nopri", "print", "firstonly",
				"unconditional", "conditional", "omitted")
		# remove any of the remaining words
		x <- killRegex( x, nmKey)
		
		### SKIP may be used instead of DROP in control files
		x <- gsub( "SKIP", "DROP", x)
		### at this point, only variables names are left
		columns <- .readValues( x , what = "character")
		
		columns <- paste( columns, collapse = ", ")

		re = data.frame( File = fileName, Columns = columns, NoHeader = noHeader,
				stringsAsFactors = FALSE, firstOnly = firstOnly, append = append)
        if (!is.null(format.of.table)) attr(re,'table.format') = format.of.table
        re
	}
	
	# extract the releveant information for each tab section
	out <- lapply( tabSec, .extractTabInfo)
	out <- do.call( rbind, out )
	out$File <- as.character( out$File )    # because data frame makes factors
	if(!is.null(path)) out$File <- file.path( path, out$File )
	out$Columns    <- as.character( out$Columns ) # because data frame makes factors
	row.names(out) <- 1:nrow(out)
	out	
}
