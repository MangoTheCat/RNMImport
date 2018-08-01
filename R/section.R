
sectionStart <- function( 
		section = "PRO", 
		text, 
		pattern = sprintf("^%s%s", sep, section ), 
		sep= "\\$", 
		... 
){
	grep( pattern , text, ... )
}

#' Used to split control file into sections.  Sections are determined
#' by checking for "$"
#' @param text control file contents
#' @param startSection [C,1] token that demarcates beginning of the section to process necessary because some sections 
#' have multiple entries (e.g. multiple $THETA sections)		 
#' @param endSection [C,1] token for section end
#' @param oneline some sections should end up occupying only one line.  this is a flag that forces this 
#' @param startpattern [C,1] full pattern for section start regexp searches, generally unused as "startSection" is sufficient
#' @param endPattern [C,1] full pattern for section end regexp searches, generally unused as "startSection" is sufficient
#' @param stripout logical flag - remove the $* bits ?
#' @param as.list logical flag - return section result as a list?
#' @param remove.comments - self explanatory
#' @param glue 
#' @param sectionSep [C, 1] token that precedes section breaks
#' @param clean 
#' @param ... 
#' @title section
#' @return A list of character vectors, where each element of the list is a section of the control file
#' @author Mango Solutions

# Originally by R. Francois

section <- function( 
		text,  
		startSection = NULL,
		endSection = NULL,
		oneline = FALSE,   
		startpattern = sprintf("^%s(%s)", sectionSep, paste(startSection, collapse="|") ) ,
		endpattern   = sprintf("^%s(%s)", sectionSep, paste(endSection, collapse="|") ) ,
		stripout = oneline, # remove the $* things
		as.list = TRUE,
		remove.comments = FALSE,
		glue = FALSE,
		sectionSep = "\\$",
		clean = FALSE,
		...  # >> strsplit 
)
{
	# TODO: More comments
 
	startPos <- if(is.null(start)) 1 else sectionStart( text = toupper(text), pattern = startpattern )
	
	out <- list( )
	l.text <- length(text)
	
	for( index in seq(along=startPos) )
	{
		
		if(index == length(startPos))
			textEnd <- l.text
		else
			textEnd <- startPos[index+1]-1
		remainingText <- text[ seq( from = startPos[index], to = textEnd ) ]
		out.index <- if(oneline) remainingText[1] else 
				if( is.null(end) || length(remainingText)==1 || !length(grep(endpattern, remainingText[-1]))) remainingText 
				else {
					remainingText[ 
							1:sectionStart( text = remainingText[-1], pattern = endpattern )[1] ]    
				}
		if(glue){
			out.index <- paste(  out.index, collapse = " ")
		}
		if(stripout) {
			out.index <- gsub( sprintf("%s[[:alpha:]]*[[:space:]]*", startpattern), "", out.index, ignore.case = TRUE)
			out.index <- negGrep( "^[[:space:]]*$", out.index, value =  TRUE )
		}
		if( remove.comments){
			out.index <- commentSplit( out.index, only.text = TRUE)
		}
		if( clean ){
			out.index <- stripBlanks( out.index )
		}
		out[[index]] <- out.index
	}                      
	if(!length(out)) NULL else if( as.list) out else unlist( out ) 
}



partitionByProblem <- function(txt, allowSuperProb = FALSE)
{	
	superProblemStarts <- sectionStart(section = "SUPER", text = txt)
	# check for the presence of super problems
	if(length(superProblemStarts) > 0 )
	{
		if(allowSuperProb)
			RNMImportWarning("Superproblems detected in text.  These are currently not handled\n", call = match.call())
		else
			RNMImportStop("Superproblems detected in text, halting\n", call = match.call())
	}
	problemStarts <- sectionStart(text = txt)
	logMessage(logName = "lowLevelParse", paste("In partitionByProblem, problems found on lines:", problemStarts))
	stopifnot(length(problemStarts) > 0)
	# handle a single problem differently
	if(length(problemStarts) == 1)
		return(list(txt))
	problemEnds <- c(problemStarts[-1] - 1, length(txt))
	lapply(seq_along(problemStarts), function(i) txt[problemStarts[i]:problemEnds[i]] )
	# lapply(head(problemStarts, -1), )	
}


sectionTitles <- function(x, substring = TRUE, start = 1, length = 3){
	titles <- unique( ogrep( "^[[:blank:]]*\\$([[:alpha:]]+)", x, filter = "\\1" ) )
	if( substring ) substring( titles , start, length ) else titles
}  
