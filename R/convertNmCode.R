#' Converts NONMEM modelling code to executable R code, returning either a list or vector of R commands
#' @title Convert NONMEM code to R 
#' @param model NONMEM "code" to convert.  May be a full PRED statement
#' @param idCol identifier column
#' @param trialCol 
#' @param mixCode 
#' @param debug  
#' @note Based on code originally by R. Francois
#' @return A list (or vector) of expressions holding the converted "models"
#' @author Mango Solutions

convertNm <- function(
		model, 
		idCol = "ID", 
		trialCol  = "TRIAL", 
		mixCode = NULL
){
	  
	# TODO: change to commentPop?
	
	logMessage(logName = "lowLevelParse", 
			paste("Parsing code in convertNm:\n", paste(model, collapse = "\n")))
	
	model <- gsub("[\";].*$", "", model) # Look for semi colons (comments)

	model <- negGrep( "(ABORT|EXIT)", model, value = TRUE ) # Remove EXIT and ABORT statements
	model <- stripBlanks( model ) # Remove spaces
	model <- gsub( "[[:space:]]*\\(", "(", model) # Replace space bracket with bracket
	
	### this is where things happens :                 
	#                                                             hold the sub                         
	#                                                           /
	#             ->    IF        get sub        is there a then\
	#                                                             only play the current line
	#                 
	#             ->    ELSE     invert sub
	#  what kind 
	#  of line    ->    ELSIF    update sub
	#
	#             ->    ENDIF    reset sub
	#
	#             ->    nothing  (play with the line and the current sub)
	#
	iflevel   <- 0              # used with nested IF
	current.subscript <- NULL   # subscript being used at the moment
	all.subs <- NULL               
	outModel <- c()             # model built
	iftarget <- NULL
	oktarget <- NULL
	linesToAdd <- NULL          # lines to add (if some variables are not here, ...)
	
	# cycle through the lines of the "model"
	for(i in 1:length(model)){
		currentLine <- model[i]  
		newline <- ""
		
		# is this a IF command ?
		if( any(regexMatches("^IF", txt = currentLine) ))
		{
			# TODO: it seems that "removeBrackets" does not actually exist as a function.  Need
			# to add this back in , written properly!
			
			# current.subscript <- removeBrackets( currentLine, method = "include" )[1]
			current.subscript <- bracketPop(currentLine, inPlace = FALSE)$op.out
			all.subs <- current.subscript
			
			# if the line ends with "THEN"
			if(any(regexMatches("THEN$", txt = currentLine)))
			{
				iflevel <- 1
			} 
			else 
			{
				# get everything after the IF(..) bit 
				endLine <- gsub("^IF([^)]*)", "", currentLine)
				tmp <- insertSubscript( endLine, current.subscript )
				newline <- tmp$line 
				newtarget <- tmp$target
				
				if(!(newtarget %in% oktarget)) 
				{
					linesToAdd <- c(linesToAdd, paste(newtarget, " <- 0 # ", 
									newtarget, " missing from '",currentLine,"'", sep = ""))
					oktarget <- c( oktarget, newtarget)				
				}
			}
			
		} else if( length(grep("^ENDIF" , currentLine) )>0){
			# reset
			current.subscript <- NULL
			all.subs <- NULL
			iflevel <- 0
		} else if( length(grep("^ELSEIF", currentLine) ) > 0){
			# newsub <- removeBrackets( currentLine, method = "include" )[1]
			newsub <- bracketPop(currentLine, inPlace = FALSE)$op.out
			current.subscript <- paste( "!(", all.subs,")", sep = "", collapse = ".AND.") 
			current.subscript <- paste( current.subscript, ".AND.(", newsub,")", sep = "" ) 
			all.subs <- c( all.subs, newsub )
			newline <- ""
			
		} else if( length(grep("^ELSE"  , currentLine)) > 0 ){
			## invert the subscript
			current.subscript <- paste( "!(", current.subscript, ")", sep= "")
			iflevel <- 1
			newline <- ""
		} else {
			
			if(iflevel == 1){
				tmp <- insertSubscript( currentLine, current.subscript )
				newline <- tmp$line                
				newtarget <- tmp$target
				if(!(newtarget %in% oktarget)){ 
					linesToAdd <- c(linesToAdd, paste(newtarget, " <- 0 # ", 
									newtarget, " missing from ' ",currentLine," '", sep = "")) 
					oktarget <- c(oktarget, newtarget)
				}
			} else {
				tmp <- insertSubscript( currentLine, NULL) 
				newline <- tmp$line
				oktarget <- c(oktarget, tmp$target)
			}
			
		}
		outModel <- c( outModel, newline)
	}
	
	### add the NEWIND column if necessary
	
	if( any(regexMatches(rx = "NEWIND", model)) )
	{
	
		outModel <- c( sprintf("NEWIND <- makeNewindColumn( %s ) ", idCol), outModel )
	}
	### add the MIXNUM column
	# MIXNUM NSPOP
	if ( any( regexMatches(model, rx = "MIXNUM")) && !is.null(mixCode) && any(regexMatches(rx = "NSPOP", mixCode)) ) { 
		idxNspop <- grep( "NSPOP", mixCode )
		if( length(idxNspop) < 1 ) RNMImportStop("No NSPOP variable in the $MIX statements", call = match.call())
		NSPOP    <- as.numeric( gsub( ".*=", "", mixCode[ idxNspop ]  ) )
		mixCode  <- mixCode [ - idxNspop ]
		mixCode  <- gsub("([^=]*)=", "\\1 <- ", mixCode ) 
		
		checkP   <- mixCode[ grep( "^ *P", mixCode ) ]
		checkP   <- gsub( "\\).*", "", checkP )
		checkP   <- gsub( "P\\(", "", checkP )
		checkP   <- unique( as.numeric( checkP ))
		if( length(checkP)!=NSPOP || !all(1:NSPOP == checkP) ) RNMImportStop("not enough P() assignments in the $MIX statements\n", call = match.call())
		for( ns in 1:NSPOP) mixCode <- gsub( paste("P(",ns,")", sep = ""), paste("P",ns,sep= ""), mixCode )
		outModel <- c( mixCode, paste('MIXNUM <- makeMixNumColumn(',idCol,',',trialCol, ',', paste('P', 1:NSPOP, collapse = ", ", sep = ""),') ' ) , outModel)
	}
	outModel <- c( linesToAdd, outModel)           # add variables that were missing
	outModel <- stripBlanks( outModel )           # remove empty lines
	outModel <- convertNmFunctions( outModel )     # EXP( -> exp(, ...
	outModel <- convertNmOperators( outModel )     # .AND. -> ) & ( ...  
	outModel <- convertNmVariables( outModel )     # THETA(1) -> TH1 ...
	outModel <- gsub( "\\<F\\>", "FX", outModel )  # F -> FX
	outModel
	
}

### convert NONMEM operators to R equivalent code                             
convertNmOperators <- function(model, logical = TRUE){
	model <- gsub("\\.EQ\\."   , " == "  , model)
	model <- gsub("\\.NE\\."   , " != "  , model)
	model <- gsub("\\.NQ\\."   , " != "  , model)
	model <- gsub("\\.LT\\."   , " < "   , model)
	model <- gsub("\\.GT\\."   , " > "   , model)
	model <- gsub("\\.GE\\."   , " >= "  , model)
	model <- gsub("\\.LE\\."   , " <= "  , model)
	if( logical ){
		model <- gsub("\\.OR\\."   , ") | (" , model)
		model <- gsub("\\.AND\\."  , ") & (" , model)
		model <- gsub("\\[", "[(", model )
		model <- gsub("\\]", ")]", model )
	}
	model
}

### convert NONMEM FUNCTIONS to R functions                                   
convertNmFunctions <- function(model){
	model <- gsub( "LOG10\\(", "log10(", model)
	model <- gsub( "LOG\\("  , "log(", model)
	model <- gsub( "EXP\\("  , "exp(", model)
	model <- gsub( "SQRT\\(" , "sqrt(", model)
	model <- gsub( "ABS\\("  , "abs(", model)
	model <- gsub( "ASIN\\(" , "asin(", model)
	model <- gsub( "ACOS\\(" , "acos(", model)
	model <- gsub( "ATAN\\(" , "atan(", model)
	model <- gsub( "SIN\\("  , "sin(", model)
	model <- gsub( "COS\\("  , "cos(", model)
	model <- gsub( "TAN\\("  , "tan(", model)
	model <- gsub( "\\*\\*"  , " ^ ", model)
	model <- gsub( "\\*"     , " * ", model)
	model <- gsub( "/"       , " / ", model)
	model <- gsub( "\\+"     , " + ", model)
	model  
}   

### take a line as "a=b" and a subscript c, and transform it to :  a[c] <- b
insertSubscript <- function( line, subscript = NULL ){
	eqpos     <- regexpr( "=", line)
	leftpart  <- gsub(" ", "", substring( line, 1        , eqpos - 1) )
	rightpart <- substring( line, eqpos + 1)
	
	if( is.null(subscript))  line <- paste( leftpart, " <- ",rightpart, sep = "" ) 
	else line <- paste( leftpart, "[", subscript, "] <- ",rightpart, sep = "" )  
	
	list( line = line, target = leftpart)
	
}

### convert NONMEM special variables to R equivalents                         
convertNmVariables <- function(model){
	vars  <- c("THETA", "ETA", "EPS", "ERR", "P")
	svars <- c("TH"   , "ETA", "EPS", "EPS", "P")
	
	for( i in seq(along = vars)){
		model <- gsub( sprintf( "%s\\(([[:digit:]]+)\\)", vars[i] ),  
				sprintf( "%s\\1", svars[i] ),  model)
	}
	
	model
}

### Creates the MIXNUM variable 
makeMixNumColumn <- function(ID, TRIAL, ...) {
	probDf <- as.data.frame(list(...))
	names(probDf) <- paste("P", seq(along = probDf), sep = "")
	myData <- data.frame(TRIAL, ID)
	myData$origOrder <- 1:nrow(myData)
	
	probData <- cbind(myData, probDf)
	probData <- probData[!duplicated(probData[, 1:2]), ]
	
	ma <- match( myData[,1:2] , as.data.frame(probData[,1:2] ))
	
	MIXNUM <- apply(as.matrix(probData[,  - (1:3), drop = F]),
			1, function(x) sample(seq(along = x), 1, prob = x))
	
	MIXNUM[ ma ]
	
}

### makes the NEWIND variable based on an "ID" variable
makeNewindColumn <- function( id, repIsNew = TRUE){
	NEWIND <- c(0, 2 - 1 * ( diff( id ) != 0 ) )
	# Replace 1s with 2s if repeated subjects are allowed
	if( !repIsNew ) NEWIND[NEWIND == 1 & duplicated(id)] <- 2
	NEWIND
}


