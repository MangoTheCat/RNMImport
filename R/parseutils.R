

negGrep <- function( pattern ,text, value = FALSE, ...)
{
	grep.out <- grep( pattern, text, value = FALSE, ...)
	all.idxs <- seq(along = text)
	idxs <- if( length(grep.out) == 0 ) all.idxs else setdiff(all.idxs , grep.out )
	if( value ) text[idxs] else idxs
}

#' similiar to "grep -o" in UNIX: returns only the sections of a vector of 
#' strings that match a regular expression 
#' @param pattern regular expression to search for 
#' @param text vector of strings in which to search
#' @param filter [C,1] replaces matched instances with this string.
#' @param ignore.case makes regexp match case-insensitive if true 
#' @param perl logical flag - use perl conventions for string matching? 
#' @title "grep -o"
#' @return A vector of segments of texts that match "pattern" 
#' @author Mango Solutions
#' 

# Original code by R Francois

ogrep <- function( pattern, text, filter = NULL, ignore.case = TRUE, perl = FALSE )
{
	grep.out <- grep( pattern , text, value = TRUE, ignore.case = ignore.case, perl = perl)
	# if no matches, return empty char vector
	if( length(grep.out) == 0) 
		return( grep.out)
	
	rx.out <- regexpr( pattern, grep.out, perl = perl )
	out <- substring( grep.out, rx.out, rx.out + attr(rx.out, "match.length") - 1)
	if(!is.null(filter)) 
		out <- gsub(pattern, filter, out, ignore.case = ignore.case, perl = perl)
	out
}

##################################################################
# pop functions:
# a collection of functions for identifying patterns in string vectors, then either in-place removing them from the string
# or returning the strings with the pattern removed
# Originally created by Romain Francois for the first RNONMEM, cleaned and augmented by F. Gochez
##################################################################

##################################################################
# pop
# searches for a pre-specified text pattern in a set of strings and then either returns those strings without 
# the text, together with a certain return value (controlled by "mode"), or directly remove the text pattern "in place"
# (if inPlace = TRUE)
# Author: Mango Solutions
# Added: Dec 22 2008
# Last modified: Dec 22 2008
# parameters -
# @ txt - [C,+] the text to process
# @ option - [C,+] the option to search for, e.g. "PRINT", "NOPRINT", etc.
# @ mode - [C,1] Controls the type of data that is returned.  If "logical", a TRUE if the option is found, FALSE otherwise
#				 If "equal", the option followed by an equal sign, if "brackets" extract the object held in brackets, 
# @ inPlace - [L,1] Logical flag.  If TRUE, modifies the input text "in place", otherwise it is returned as a component in a 
# list
# @ absent - What to return if the string is missing
# @ ignore.case - If TRUE, ignores case
# @ sep - [C,1] character to use a separator between key and value
# @ removeBrackets - [L,1] 
# @ numeric - [L, 1] Logical flag.  If TRUE, will return as numeric
##################################################################

pop <- function( 
		txt, option, mode = c( "logical", "equal", "brackets","comments" ),
		inPlace = TRUE, absent = if( mode == "logical") FALSE else NULL ,	ignore.case = TRUE, 
		shortcut = FALSE, sep = "=", 
		.depth = 1, removeBrackets = (mode == "brackets"),
		numeric = FALSE,
		... 
)
{
	
	if(inPlace ) 
		nameTxt <- deparse( substitute( txt ))  
	
	### need spaces after closing brackets
	txt <- gsub( "\\)[[:space:]]*", ") ", txt )
	
	### expand the regex if dealing with a shortcut
	#   FIX instead of FIXED, etc, ....
	mode <- match.arg( mode )
	if( shortcut && mode == "equal" )
	{
		option <- sprintf("[^[:space:]]*%s[^[:space:]=]*", option )
	}
	
	### build the regex to search for
	rx <- switch( mode, 
			"equal"    =  sprintf( "\\<%s%s([^[:space:]]*)[[:space:]]*", option, sep )   ,
			"logical"  =  sprintf( "\\<%s\\>", option )   , 
			"brackets" =  "(\\([^\\)]*\\))", 
			"comments" = ";(.*$)")
	grep.out <- grep( rx, txt, ignore.case = ignore.case, ... )
	
	op.out <- 
			if( length(grep.out) )
			{
				out <- switch( mode, 
						"logical"  = TRUE, 
						"equal"    = gsub( "['\"]", "", gsub( sprintf("^.*%s.*$", rx), "\\1", txt[grep.out], 
										ignore.case = ignore.case, ... ) ), 
						"brackets" = sub( sprintf("^[^\\(\\)]*%s.*$", rx), "\\1", txt[grep.out], 
								ignore.case = ignore.case, ...), 
						"comments" = {
							com <- rep("", length(txt)) 
							com[grep.out] <- sub( sprintf("^.*%s", rx), "\\1", txt[grep.out])
							com
						})
				txt <- sub( sprintf("[[:space:]]*%s[[:space:]]*", rx), " ", txt)
				if(removeBrackets){
					out <- gsub("[\\(\\)]", "", out)
				}
				out
			} 
			else absent
	if( numeric) 
		op.out <- as.numeric(op.out)
	if( inPlace ) 
	{	
		assign( nameTxt, txt, parent.frame(.depth) )
		return(op.out)
	}
	list(op.out = op.out, txt = txt)
}

##################################################################
## colonPop - wrapper for pop, extracts 2 sides of the expression divided by ":" 
## equalExpressionPop - Looks for the pattern "<x> = <y>", returns <y>, and then removes the pattern.  "=" can be replaced
# with any expression 
## bracketPop - Looks for content inside brackets, can return content with or without brackets, only used in importNmModSim 
## logicalPop - Performs a "pop", but coerces the result to a logical
## commentPop - looks for comments, extracts semicolon and everything after it
## fortranPop - strips out raw Fortran code in NONMEM control files
#  Author: Mango Solutions
# Added: Dec 25 2008
# Last modified: Dec 25 2008
# parameters -
# @ ... - passed straight to pop
# @ .depth - passed straight to pop
##################################################################

colonPop <- function(..., .depth = 2) pop( ..., mode = "equal",  .depth = .depth, sep= "[:[:space:]]*")

equalExpressionPop <- function(..., .depth = 2, sep = "[[:space:]]*=[[:space:]]*")
{
	pop( ..., mode = "equal"  ,  .depth = .depth, sep = sep  )
}

bracketPop  <- function(..., .depth = 2) pop( ..., mode = "brackets",  .depth = .depth  )

logicalPop  <- function(..., inPlace = FALSE, .depth = 2) {
	out <- pop( ..., inPlace = inPlace, mode = "logical" ,  .depth = .depth )
	if(inPlace)
		if(!is.null(out)) out <- return(as.logical( out ))
	else
		if(!is.null(out$op.out)) 
			out$op.out <- as.logical(out$op.out)
	out
}

commentPop  <- function(..., .depth = 2) pop( ..., mode = "comments" , .depth = .depth  )

fortranPop  <- function(..., .depth = 2) pop( ..., mode = "comments" , .depth = .depth, sep = "['\"]"  )

##################################################################
# ynPop
# In NONMEM, some options are enabled by including a keyword such as PRINT, and are disabled with another 
# one such as NOPRINT.  However, other options are disabled by merely excluding the keyword, or for example REWIND and NOREWIND, 
# if it does not find any, returns the default value

# Author: Mango Solutions
# Added: Dec 25 2008
# Last modified: Dec 25 2008
# parameters -
# @ txt - [C,+] the text to process
# @ option - [C,+] the option to search for, e.g. "PRINT", "NOPRINT", etc.
# @ default - default value to return 
##################################################################


ynPop <- function( 
		txt,                                         
		option,                                     
		yes.prefix = "",
		no.prefix = "NO",
		default = TRUE,
		inPlace = FALSE
)
{

	nameTxt <- deparse(substitute(txt))
	
	# alternative way of setting opt: REWIND|NOREWIND
	# this sets the default value to TRUE
	if(length(grep("\\|", option))){
		spli <- strsplit(option, "\\|")[[1]]
		# find the actual option name
		option <- gsub( sprintf("^%s", yes.prefix), "", spli )
		option <- gsub( sprintf("^%s", no.prefix), "", option )
		if(option[1] == option[2] ) option <- option[1] else
			stop("unable to guess option name")		
		yes.var <- paste(yes.prefix, option, sep = "") # yes option, ex: REWIND
		default <- yes.var == spli[1]
	}
	else
		yes.var <- paste(yes.prefix, option, sep = "") # yes option, ex: REWIND		
	no.var  <- paste(no.prefix, option, sep = "")
	if(inPlace)
	{
		no  <- logicalPop( txt, no.var, absent = NULL, inPlace = TRUE)
		yes <- logicalPop( txt, yes.var, absent = NULL, inPlace = TRUE )
		assign( nameTxt, txt, parent.frame())
	}
	else
	{
		out <- logicalPop(txt, no.var, absent = NULL, inPlace = FALSE)
		no <- out$op.out
		out <- logicalPop(out$txt, yes.var, absent = NULL, inPlace = FALSE)
		yes <- out$op.out
	}
	
	# returns the default value if none are present (0) or both are present (2)
	if( (is.null(yes)+is.null(no))!=1 ) 
		default 
	else 
	{
		if(is.null(yes)) !no else if(is.null(no)) yes
	}
	
}

##################################################################
# commentSplit
# Splits a set of strings along comment delimiter.
# Author: Mango Solutions
# Added: Jan 2 2009
# Last modified: Jan 2 2009
# parameters :
# @ txt [C,+] - A character vector holding the text to split
# @ pattern - A regular expression that fully describes the section markets, such as $PROBLEM
# @ sep - The prefix for a section separator
# returns - Indices where the sections begin
##################################################################

commentSplit <- function( 
		txt, 
		commentChar=";", 
		# FG: pattern is never used, it forms the regular expression to 
		# forms the regexp used to find locations where split is done
		pattern = sprintf( "[[:space:]]*[%s][[:space:]]*", commentChar ),  
		strip = FALSE,    #FG: if TRUE, strips out the regular expression in stripRx
		stripRx = "^[[:space:]]*$", 
		clean = FALSE,  #FG: if TRUE, remove blanks 
		only.text = FALSE,  # FG: return only text or only comments?
		only.comments = FALSE,
		... # additional arguments passed to strsplit
){
	
	txtSplit   <- strsplit( txt, pattern, ... )
	txt        <- sapply( txtSplit, "[", 1 )  
	comments   <- sapply( txtSplit, function(x) paste( x[-1], collapse="") )
	
	if( strip ){
		txtSplit <- negGrep( stripRx, txt, value = TRUE )
		comments <- negGrep( stripRx, comments, value = TRUE )
	}
	if( clean ){
		txtSplit <- stripBlanks( txtSplit )
		comments <- stripBlanks( comments )    
	}
	txt[is.na(txt)] <- ""
	comments[is.na(comments)] <- ""
	if( only.text ) txt else if(only.comments ) comments else
		list( text = txt, comments = comments )
	
}

##################################################################
# stripBlanks
# 
# Author: Mango Solutions
# Added: Jan 2 2009
# Last modified: Jan 2 2009
# parameters :
# @ section - [C,1] A string demarcating the section (e.g. "PROBLEM", "INPUT")
# @ text - [C, +] A character vector holding the text to check for sections
# @ pattern - A regular expression that fully describes the section markets, such as $PROBLEM
# @ sep - The prefix for a section separator
# returns - Indices where the sections begin
##################################################################


stripBlanks <- function( 
		txt,                            #@ [C,+] the text to modify 
		trim = TRUE,                    #@ [L,1] doing any trimming (replace multi space with single space)
		leading = TRUE,                 #@ [L,1] remove leading spaces
		trailing = TRUE,                #@ [L,1] remove trailing spaces
		shrink = TRUE,                  #@ [L,1] shrink spaces around = signs
		trimRx = "[[:space:]]+", 
		trailingRx = "[[:space:]]*$", 
		leadingRx  = "^[[:space:]]*", 
		remove.empty = FALSE
){
	
	if( leading )  txt <- gsub(leadingRx, "", txt)
	if( trailing ) txt <- gsub(trailingRx, "", txt)
	if( trim )     txt <- gsub(trimRx, " ", txt)
	if( shrink )   txt <- gsub("[[:space:]]*=[[:space:]]*", "=", txt)
	txt <- if( remove.empty ) txt[txt!=""] else txt
	txt
}

##################################################################
# regexSplit
# Splits a string along a regular expression
# Author: Mango Solutions
# Added: Jan 5 2009
# Last modified: Jan 5 2009
# parameters :
# @ txt - [C,+] A vector of strings
# @ rx - [C, 1] A regular expression
# returns - Character vector of the components
##################################################################

regexSplit <- function(txt, rx)
{
	out <- unlist( strsplit( txt, rx) )
	out[out!= ""]
	
}

##################################################################
# killRegex
# Removes a regular expression from a set of strings
# Author: Mango Solutions
# Added: Jan 5 2009
# Last modified: Jan 5 2009
# parameters :
# @ txt - [C,+] A vector of string
# @ rx - [C, +] A vector of regular expression strings
# @ ignore.case [L,1] - should case be disregarded?
# @ rmBlanks [L,1] - should blanks be removed from the result before returning? 
# returns - txt with all regular expressions removed
##################################################################

killRegex <- function(txt, rx, ignore.case = TRUE, rmBlanks = FALSE, ...)
{
	for(rxToKill in rx)
		txt <- gsub(rxToKill, '', txt, ignore.case = ignore.case,...)
	if(rmBlanks)
		stripBlanks(txt)
	else
		txt
}

##################################################################
# .rmSpaceInBrackets
# removes space from an expression inside a pair of ROUND brackets
# Author: Mango Solutions
# Added: Jan 5 2009
# Last modified: Jan 5 2009
# parameters :
# @ x - [C,+] A vector of strings
# returns - Character vector with the strings removed
##################################################################


.rmSpaceInBrackets <- function(x){
	x <- gsub( "[[:space:]]*,[[:space:]]*", ",", x )
	x <- gsub( "\\([[:space:]]*"          , "(", x )
	x <- gsub( "[[:space:]]*\\)"          , ")", x )
	x  
}

#' Reads a set of values from a string of characters, in a given format, by essentially calling 
#' scan on a textConnection.  Scanning is wrapped in a "try" statement 
#' (e.g. character, numeric)
#' @param txt character vector of text to process 
#' @param quiet Passed to the "try" function 
#' @param what Passed to "scan"
#' @param ... additional parameters passed to "scan"
#' @return vector
#' @author Mango Solutions

.readValues <- function( txt, quiet = TRUE, what = "character",  ... )
{
	dataCon <- textConnection( txt )
	dataVal <- try(scan( dataCon , quiet = quiet, what = what, ... ))
	dataVal[ grep("\\.\\.+", dataVal) ] <- "0"
	if (inherits(dataVal,"try-error")) 
	{
		msg <- paste("\n Failed to read the following values:", txt, collapse = "\n")
		RNMImportStop(msg, match.call())
	}
	close(dataCon)
	dataVal
}

##################################################################
# regexMatches
# removes space from an expression inside a pair of ROUND brackets
# Author: Mango Solutions
# Added: Jan 8 2009
# Last modified: Jan 8 2009
# parameters :
# @ txt - [C,+] A vector of strings
# @ rx - [C,1] - A regular expression
# returns - logical vector indicating which elements of txt match 
##################################################################


regexMatches <- function(txt, rx)
{
	regexpr(pattern = rx, text = txt) > 0	
}

# Return a "blank" if x is NULL

nullIfBlank <- function(x)
{
	if(all(x == ""))	NULL 
	else x
}

#' generates a set of sequences that start at x[i] and end at x[i+1], where i varies from 1 to length(x)-1.  If txt is not
#' NULL, in addition it will subsets of txt created by indexing it with these sequences
#' @param x integer vector
#' @param txt 
#' @return List of sequences, as described above
#' @author Mango Solutions

lseq <- function( x, txt = NULL ){
	out <- mapply( seq, length.out = diff(x), from = head(x, -1) )
	if( is.null(txt) ) 
		out 
	else
		sapply( out, function(x) txt[x] )
}  

#' "Splits" a given vector along a vector of indices. The split content is returned as a list ranging between each index,
#' with each index 
#' @title split a vector into a list of sub-vectors
#' @param x The vector to split
#' @param indices a set of integer indices along which to split
#' @param includeStart should the first element of the vector be included automatically? 
#' @param includeEnd should the last element of the vector be included automatically?
#' @return A list of split elements of x.  These will be of the form x[index_n:(index_n - 1)] for each index
#' @author Mango Solutions

splitVector <- function(x, indices, includeStart = FALSE, includeEnd = FALSE)
{
	RNMImportStopifnot(length(indices) > 1)
	RNMImportStopifnot(all(diff(indices) > 0))
	RNMImportStopifnot(tail(indices, 1) <= length(x) )
	
	# if these indices should be forced, add them
	if(includeEnd) indices <- c(indices, length(x)+1)
	if(includeStart) indices <- c(1, indices)
	 # remove any duplicates
	indices <- unique(indices)
	
	splitList <- vector(mode = "list", length = length(indices) - 1)
	# loop through the indices and break into chunks
	for(n in 1:(length(indices) - 1))
	{	
		startIndex <- indices[n]
		endIndex <- indices[n+1] - 1
		splitList[[n]] <- x[startIndex:endIndex]
	}
	
	splitList	
	
}

extractBalanced <- function(stream, leadingChar='ACCEPT\\s*=\\s*'){
    orig = stream
    # Assume comments only happened at the end of stream, and remove all possible comments
    # if we assume comments can happen interior of streams, it will be terrificly hard to deal with

    # to process the following case:
    #   ACCEPT = ( A .NE. 'abc;def')
    # before we remove ;.*$, we have to first clear our strings in ""
    # and we also use this to deal with ")" or "("

    i = 1
    lstream = nchar(stream)
    quote.sym = NULL
    while (i <= lstream){
        if (is.null(quote.sym)){
        # not in quote
            if (substring(stream,i,i)=='"' || substring(stream,i,i)=="'" ) {
                quote.sym = substring(stream,i,i) 
                substring(stream,i,i)=' ' # replace string in quotes by ' '
            }
        } else {
            if (substring(stream,i,i)==quote.sym) { 
                # if in quote state and met a pairing one, exit the quote state
                quote.sym = NULL
            }
            substring(stream,i,i)=' ' # replace string in quotes by ' '
        }
        i = i+1
    }

    # comment position
    pos.comment = regexpr(';.*$','', stream)

    # extract the first appearance of leadingChar=(....) in the stream, 
    # we already protect it from "" or ''
    startpos.tag = regexpr(leadingChar, stream)
    if (startpos.tag <= 0 || (pos.comment>0 && startpos.tag >= pos.comment)) {
        # the leading tag is not found in string, keep the original stream untouched
        return( c('', orig) )
    }

    # Just before '('
    i = startpos.tag + attr(startpos.tag, 'match.length') - 1 
    if (pos.comment > 0) {
        lstream = pos.comment - 1
    } else {
        lstream = nchar(stream)
    }
    nextchar = function(){
        if (i > lstream) return(NULL)
        substring(stream,i+1,i+1)
    }
    consumeone = function(){
        re = nextchar()
        if (!is.null(re)){
            i <<- i+1
            return(re)
        }
        re
    }
    contents = ''
    par.bal = 0
    balanced = TRUE
    # according to NONMEM manual, the contents must be surrounded in () pairs
    while(par.bal==0 && !is.null(nextchar())){
        if (nextchar()=='(') 
            par.bal = par.bal + 1
        consumeone()
    }
    if (!is.null(nextchar())){
        # if we do find the starting '('
        startpos.parenthess = i
        while(par.bal>0 && !is.null(nextchar())){
            if (nextchar()=='(') {
                par.bal = par.bal + 1
            } else {
                if (nextchar()==')') {
                    par.bal = par.bal - 1
                }
            }
            consumeone()
        }
        if (substring(stream,i,i) !=')') {
            balanced = FALSE
            warning(sprintf('Unbalanced parenthess found in %s .', orig))
        }
        contents = substring(orig,startpos.parenthess,i)
        orig = paste( substring(orig,1,startpos.tag-1),
                      substring(orig,i+balanced) , sep='')
    }
    c(contents,orig)
}
