
#' Splits a single block of #METH statements into "sections".  This is similar to sectionLst, but only for a
#' single block delimeted by #METH in NONMEM 7
#'
#' @title Split method text block into sections
#' @param [C,+] methodContents Line of text in a #METH statement (includes the #METH line)
#' @return [L, +] a named list of split text, with names corresponding to section names
#' @author Mango Solutions
#' @noRd

sectionMethodBlock <- function(methodContents)
{
	#
	SECTIONSEP <- "1"
	
	# strip blank lines
	
	methodContentsClean <- negGrep("^[[:space:]]*$", methodContents, value = TRUE) 
	
	# pattern that delimits sections, e.g.
	
	#1
	# ************************************************************************************************************************
	
	# SECTIONSEPPATTERN <- paste("^[[:space:]]*", SECTIONSEP, "[[:space:]]*\n[[:space:]]*", paste(rep("\\*", times = 15), collapse = ""), sep = "" )
	SECTIONSEPPATTERN <- paste("^[[:space:]]*", SECTIONSEP, "[[:space:]]*$", sep = "" ) 
	
	sectionStarts <- grep(methodContentsClean, pattern = SECTIONSEPPATTERN)
	# the following logic eliminates as section starts those lines that are NOT followed by 
	# " *********** " (or nothing).  These are not real section starts
	x <- regexMatches(methodContentsClean[sectionStarts + 1], "[[:alnum:]]+")
	
	x[is.na(x)] <- FALSE
	sectionStarts <- sectionStarts[!x]
	RNMImportStopifnot(length(sectionStarts) >= 1, match.call())
	# grab the section "header" text block
	
	headerBlock <- methodContentsClean[1:(sectionStarts[1] - 1)]
	
	methodContentsNoHeader <- tail(methodContentsClean, sectionStarts[1] - 1)
	
	# split the rest into chunks of text that begin with "1"
	
	blockSplits <- splitVector(methodContentsClean, sectionStarts, includeEnd = FALSE)
	# now decompose those text blocks into sections using the nmTitle function
	blockSplits <- lapply(blockSplits, 
			function(x) {
				# the first line of each "split" block is a "1" delimeter, which should be removed, hence the x[-1]
				# NONMEM 7 has titles of 6 lines, rather than 5 for NONMEM VI 
				xDecomposed <- nmTitle(x[-1], titleSecLength = 6)
				y <- xDecomposed$title
				RNMImportStopifnot(length(y) <= 2)
				# second element of "title" is the actual title
				y <- if(length(y) == 2) y[2] else y
				z <- xDecomposed$text
				list(title = y, text = z)
			})
	
	# also remove the presence of lines of the form "1  "
	blockContents <- lapply(blockSplits, function(x) negGrep(x$text, pattern = paste("^", SECTIONSEP, "[[:space:]]*$", sep = "") , value = TRUE))
	names(blockContents) <- sapply(blockSplits, function(x) x$title )
	
	
	blockContents
}

# TODO: this function is overly-complex; clean it so that it no longer recurses

#' Partitions the contents of a list file into sections that can then be parsed individually
#' @param fileContents [C,+] Lines of text from a list file
#' @param sep (?) passed to .cleanRx
#' @param keep.all (?)
#' @param recurse Should the function be called recursively?  
#' @note Based on code by R. Francois
#' @return A (partially) named list with relevant sections of the lst file.
#' @author Mango Solutions
#' @noRd

sectionLst <- function( fileContents, sep, 
		keep.all = FALSE, recurse = TRUE )
{         
	
	### internal function to clean a regex
	.cleanRx <- function(x, sep){
		rx <- sprintf("^%s", sep)
		x <- gsub( rx, "", x)
		negGrep( "^[[:space:]]*$", x , value = TRUE)
	}
	
	if(missing(sep)){
		# sep <- if( fileContents %a~% "^2" ) "2" else "1"
		sep <- if(any(regexMatches(fileContents, rx = "^2") )) "2" else "1"
	}

	if( is.null(fileContents) ) return(NULL)
	
	rx <- sprintf("^%s", sep )
	start <- grep( rx, fileContents )
	appendBefore <- !1 %in% start
	appendAfter <- !length(fileContents) %in% start
	if( appendBefore ) start <- c( 1, start )
	if( appendAfter  ) start <- c( start, length(fileContents)+1 )
	
	len.sta <- length(start) - 1
	li <- list()    
	for( i in 1:len.sta){
		current <- fileContents[seq(from = start[i], to = start[i+1] - 1)]
		current <- .cleanRx( current, sep ) 
		if( sep == "2" &&  recurse ){
			current <- sectionLst( current, "1" )
		}    
		li[[i]] <- current
	}   
	
	if( sep == "1"){  
		### try to find star titles
		if( sep == "1" && len.sta > 2 ) {      
			for(i in seq(along = li) ){
				nmt <- nmTitle( li[[i]] )
				if( !is.null(nmt$title) ){
					li[[i]] <- nmt$text
					names(li)[i] <- paste(nmt$title, collapse='\n')
				}
				if( length( idx <- grep("MONITORING OF SEARCH", li[[i]])) ){
					names(li)[i] <- "MONITORING OF SEARCH"
					li[[i]] <- li[[i]][-idx]
					li[[i]] <- sectionLst( li[[i]], "0")
				}
			}
		}
		### handle repeated headers  
		headers <- c("INITIAL PARAMETER ESTIMATE", 
				"FINAL PARAMETER ESTIMATE", "STANDARD ERROR OF ESTIMATE", 
				"COVARIANCE MATRIX OF ESTIMATE", "CORRELATION MATRIX OF ESTIMATE", 
				"INVERSE COVARIANCE MATRIX OF ESTIMATE", "ERRORS")
		if( any(names(li) %in% headers)) for( tit in headers ){
				
				nli <- names(li)
				idx <- which( nli == tit )
				if(!length(idx)) next
				current <- idx + 1
				while( current <= length(nli) && is.na(nli[current]) ){
					toappend <- li[[current]]
					plus <- grep( "^\\+", toappend)
					if( length(plus) ){
						toappend <- toappend[-c(1:(plus[1]-2)) ]
					}
					
					li[[idx]] <- c( li[[idx]], toappend )
					li[[current]] <- NULL
					nli <- names(li)
				}
				
			}
	}     
	if( sep == "2" ) {
		if( !keep.all ) {
			# only keep the first subproblem
			newli <- li[[1]]
			if( length(li) > 1 ) newli <- append( newli, li[[2]][-1] )
			newli
		} else {
			header <- li[[1]]
			newli <- lapply( li[-1], append, header, after = 0 )      
			newli
		}
	} else if( length(li) > 1 ) li else li[[1]] 
}

#' Splits a chunk of report file section into its title/header and its title.
#' @title split list text into sections
#' @param txt [C,+] The input sectioned chunk of the lst file.  This is typically obtained from sectionLst or
#' sectionMethodBlocks.  See the comment block below for examples of section text 
#' @param titleSecLength [N,1] 
#' @param maxTitleLines [N,1]   
#' @return a list with two elements: "text" and "title".  The latter is the title of the section 
#' (e.g. "FINAL PARAMETER ESTIMATE"), the former is the text content without the titles
#' @author Mango Solutions
#' @noRd

# Example section text:
#
#[1] " ************************************************************************************************************************"
#[2] " ********************                                                                                ********************"
#[3] " ********************                                  FINAL PARAMETER ESTIMATE                      ********************"
#[4] " ********************                                                                                ********************"
#[5] " ************************************************************************************************************************"
#[6] " THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********"                                                                  
#[7] "            TH 1      TH 2      TH 3      TH 4"                                                                           
#[8] "         2.78E+00  3.02E-01  4.90E-01  1.14E+01"                                                                          
#[9] " OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********"                                                                  
#[10] "            ETA1      ETA2      ETA3      ETA4"                                                                           
#[11] " ETA1"                                                                                                                    
#[12] "+        1.69E-01"                                                                                                        
#[13] " ETA2"                                                                                                                    
#[14] "+        0.00E+00  1.08E-02"                                                                                              
#[15] " ETA3"                                                                                                                    
#[16] "+        0.00E+00  0.00E+00  9.42E-02"                                                                                    
#[17] " ETA4"                                                                                                                    
#[18] "+        0.00E+00  0.00E+00  0.00E+00  9.87E-02"                                                                          
#[19] " SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****"                                                                  
#[20] "            EPS1"                                                                                                         
#[21] " EPS1"                                                                                                                    
#[22] "+        1.97E-02"  

# expected output is:

# $text:
#[1] " THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********" "            TH 1      TH 2      TH 3      TH 4"         
#[3] "         2.78E+00  3.02E-01  4.90E-01  1.14E+01"         " OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********"
#[5] "            ETA1      ETA2      ETA3      ETA4"          " ETA1"                                                  
#[7] "+        1.69E-01"                                       " ETA2"                                                  
#[9] "+        0.00E+00  1.08E-02"                             " ETA3"                                                  
#[11] "+        0.00E+00  0.00E+00  9.42E-02"                   " ETA4"                                                  
#[13] "+        0.00E+00  0.00E+00  0.00E+00  9.87E-02"         " SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****"
#[15] "            EPS1"                                        " EPS1"                                                  
#[17] "+        1.97E-02"                    

# $title: `FINAL PARAMETER ESTIMATE`

nmTitle <- function( txt, titleSecLength = 5, maxTitleLines = 2 ){
	if( is.list(txt)  ) return( list( title = NULL ) )
	
	if(any(regexMatches( txt, rx = "\\*{15,}" )))
	{
		titleText <- grep("\\*{15,}", txt[1:titleSecLength], value = TRUE )
		titleText <- gsub( "\\*+", "", titleText )
		titleText <- stripBlanks( titleText, remove.empty = TRUE)
		txt <- txt[-(1:titleSecLength)]
	} 
	else 
	{
		if(any( regexMatches(txt = txt, "THERE ARE ERROR")) )
			titleText <- "ERRORS" 
		else return( list(title= NULL))
	}
	
	list( title = titleText, text = txt)
}


#' Obtains the NONMEM version information from a list file
#' @param lstContents [C,+] Contents of the lst file
#' @return A list with two elements: the version and the level
#' @author Mango Solutions
#' @noRd

nmVersion <- function(lstContents)
{
	# find the line where the version information is held
	line <- grep( "NONLINEAR MIXED EFFECTS MODEL PROGRAM", lstContents, fixed = TRUE, value = TRUE )
	
	if( length(line) == 0)
		RNMImportStop("Could not find any version information in the contents of the report file")
	# extract VERSION=
	nmVersion <- equalExpressionPop( line, "VERSION", sep = "[[:space:]]*" )
	nmLevel   <- equalExpressionPop( line, "LEVEL", sep = "[[:space:]]*")
	c( Version = nmVersion, Level = nmLevel)
}


#' partitionLstSubproblems
#' @param contents A character vector of strings holding the lines of subproblem information (ONLY!) for one simulation problem
#' @return list
#' @author Mango Solutions
#' @noRd

partitionLstSubproblems <- function(contents)
{
	# NB this handles the sub-problems of 1 problem only
	# look for lines of the form PROBLEM NO.:         1     SUBPROBLEM NO.:      1
	subprobStarts <- grep(contents,	pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*[0-9]" )
	msg <- length(subprobStarts) %pst% " subproblems found\n"
	logMessage(logName = "lowLevelParse", msg)
	
	# now cut along the subproblem starts
	if(length(subprobStarts) == 1)
		return(list(contents))
	subprobEnds <- c(subprobStarts[-1] - 1, length(contents))
	lapply(seq_along(subprobStarts), function(i) contents[subprobStarts[i]:subprobEnds[i]])
}

#' Obtain the minimum value of the objective function from the correct element of the list returned by sectionLst
#' @param item An element of the list returned by sectionLst
#' @return NULL if item is NULL, minimum value of the objective function otherwise
#' @author Mango Solutions
#' @noRd

# TODO: robustify this!

.nmMVOF <- function( item ){
	if( is.null(item)) NULL else as.numeric(gsub("[\\*[:space:]]+", "", item ))
}

# .nmMVOFNM

#' Retrieves the NONMEM variable names in a set of strings
#' @param txt vector of strings
#' @return A character vector of the NONMEM variable names
#' @author Mango Solutions
#' @noRd

.nmVariableNames <- function( txt )
{
	varRx <- "([[:alpha:]]+)[[:space:]]([[:digit:]]+)"
	txt <- gsub( varRx, "\\1\\2", txt )
	txt <- unique( unlist( strsplit( txt, "[\\|[:space:]]" ) ))
	txt <- txt[txt!=""]
	txt
}

#' Partitions the text of a report file into chunks of text for each "method" used.  This applies to
#' NONMEM 7 reports only, as they will now have separate parameter estimates, objective function values etc.
#' for different $EST methods employed.
#' @param lstProblemContents A character vector of lines of text from a NONMEM 7 report file  
#' @return A list of character vectors of split text.  Each element of the list will also have an attribute
#' called "method.name".  This will denote the actual name of the method used for that chunk. 
#' @author Mango Solutions
#' @noRd

partitionMethods <- function(lstProblemContents)
{
	METHODDELIMITER <- "#METH"
	# holds the indices of the lines where each METHOD result is held
	methodLines <- grep(lstProblemContents, pattern = METHODDELIMITER)

	if(length(methodLines) == 0)
	{
		RNMImportStop(msg = "No instances of #METH found in the report file!\n", match.call())
		
	}
	# index of the last line of the report file
	lastLine <- length(lstProblemContents)
	# all of the points at which to partition
	breakPoints <- c(methodLines, lastLine + 1)
	# list that will hold each of the #METH chunks
	methodBlocks <- vector(mode = "list", length = length(breakPoints) - 1 )
	for(i in head(seq_along(breakPoints), -1 ))
	{
		
		methodLine <- lstProblemContents[breakPoints[i]]
		# assign the name of method as an attribute to the element of the list
		# methodName <- pop( lstProblemContents[breakPoints[i]], option = "#METH", sep = ":", mode = "equal", inPlace = FALSE  )$op.out
		ignoreMatch <- gregexpr( methodLine, pattern = "[[:space:]*]\\#METH:[[:space:]*]" )
		
		methodName <- substr(methodLine, start = 1 + attr(ignoreMatch[[1]], "match.length"), stop = nchar(methodLine))
		
		methodBlocks[[i]] <- lstProblemContents[breakPoints[i]:(breakPoints[i+1]-1)]
		attr(methodBlocks[[i]], "method.name") <- methodName
	}
	methodBlocks
}
