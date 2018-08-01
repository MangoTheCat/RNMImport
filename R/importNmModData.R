

#' Parses the $DATA statement of a control file from some text 
#' @title Parse $DATA statement
#' @param txt [C,+] - vectors holding control file text 
#' @param modFile The name of the control file from which the text comes
#' @param .extract Logical flag.  If TRUE, assumes
#' @return A matrix of descriptor information
#' @author Mango Solutions
#' 

.importNmModData <- function(txt, modFile, 
        .extract = length(grep("^\\$DAT", toupper(txt))) > 0 ){
    
    ### import the $DATA section of the control file                              
    x <- if(.extract) section( txt, "DAT", "", stripout = TRUE, 
                        as.list = TRUE, glue = TRUE) else list( txt )
    
    # internal function, meant to be used within sapply only.  Parses a single line of the $DATA section
    
    .extractDataInfo <- function( dataSec )
    {
        ### remove nonmem KEYWORDS that we do not wish to import                    
        dataSec <- killRegex( dataSec,  c("CHECKOUT", "NOOPEN") )
        
        ### extract ACCEPT first
        accept = extractBalanced( dataSec )
        names(accept) = NULL
        dataSec = accept[2]
        accept = accept[1]
        ### WIDE or NOWIDE                                                          
        wide <- ynPop( dataSec, "WIDE", default = FALSE, inPlace = TRUE)
        
        ### NULL                                                                    
        null <- equalExpressionPop( dataSec, "NULL", absent = "", sep="=", inPlace = TRUE)
        
        ### REWIND, NOREWIND                                                        
        rewind <- ynPop( dataSec, "REWIND", default = FALSE, inPlace=TRUE)
        
        ### records, may be coded NRECS, NRECORDS, RECS, RECORDS                    
        records <- equalExpressionPop( dataSec, "N?RECO?R?D?S", absent = "", sep="=",inPlace = TRUE)
        
        ### hunt for the IGNORE declaration      
        # this is the regular expression for detecting IGN[ORE] statements (there may be multiple)
        #they may be as follows:
        # IGNORE or IGN=(code)
        # IGNORE=LABEL
        # IGN or IGNORE(code)
        # ignoreRegexp <- "[[:space:]]+(IGN|IGNORE)[[:space:]]*=[[:space:]]*[,\\.[:alnum:]\\(\\)\\@\\#\"=\\<\\>/']+"
        
#       ignoreRegexp <- "([[:space:]]+(IGN|IGNORE)[[:space:]]*[=]{0,1}[[:space:]]*\\([,\\.[:alnum:]\"=\\<\\>/'[:space:]]+\\)|[[:space:]]+(IGN|IGNORE)[[:space:]]*=[[:space:]]*[,\\.[:alnum:]\\@\\#\"=\\<\\>/']+)"
		ignoreRegexp <- "([[:space:]]+(IGN|IGNORE)[[:space:]]*[=]{0,1}[[:space:]]*\\([, *\\.[:alnum:]\"=\\<\\>/']+\\)|[[:space:]]+(IGN|IGNORE)[[:space:]]*=[[:space:]]*[,\\.[:alnum:]\\@\\#\"=\\<\\>/']+)"
        
        # one can also use IGN(code) or IGNORE(code)
        
        ignorePos <- gregexpr(dataSec, pattern = ignoreRegexp, ignore.case = TRUE)
        
        # the call to gregexpr returns starting positions and lengths of matches, so now we must extract the actual strings
        
        ignoreText <- substring(dataSec, ignorePos[[1]], ignorePos[[1]] + attr(ignorePos[[1]], "match.length") - 1)
        # remove all whitespace
        ignoreText <- sapply(ignoreText, gsub, pattern = "[[:blank:]]+", replacement = "")
        # extract tokens with only IGN, seperate from IGNORE
        
        ignPos <- negGrep(ignoreText, pattern = "IGNORE", value = FALSE, ignore.case = TRUE)
        ignText <- ignoreText[ignPos]
        if(length(ignPos) > 0)  
            ignoreText <- ignoreText[-ignPos]
        
        # now extact the actual ignore tokens.  These may be delimited by "IGN" or "IGNORE", so we must capture both.  Also, we seperate
        # those expressions of the form IGNOR
        

		ignoreTokens <- sapply(ignoreText, function(x) gsub(x, pattern = "IGNORE[=]{0,1}", replacement = "", ignore.case = TRUE))
        
		ignTokens <- sapply(ignText, function(x) gsub(x, pattern = "IGN[=]{0,1}", replacement = "", ignore.case = TRUE ))
      
        # replace empty strings with "NONE"
        ignoreTokens <- gsub(c(ignoreTokens, ignTokens),pattern = "^$", replacement = "NONE")
        
        # NONE should only appear on its own, but the above code might generate more than one instance, so we need to clean this
        if(any(ignoreTokens == "NONE"))
        {
            if(!all(ignoreTokens == "NONE"))
                ignoreTokens <- ignoreTokens[ignoreTokens != "NONE"]
            else
                ignoreTokens <- "NONE"
        }
        
        
        # strip out quotes and "'" 
        ignoreTokens <- sapply(ignoreTokens, gsub, pattern = "['\"]", replacement = "")        
		allIgnore <- paste(ignoreTokens, collapse = ";")
        
        # now delete the IGNORE= declarations from dataSec
        
        dataSec <- gsub(dataSec, pattern = ignoreRegexp, replacement = "", ignore.case = TRUE)
        
        ### hunt for the KEEP declaration                                           
        


        ### TRANSLATE                                                               
        translate <- equalExpressionPop( dataSec, "TRANSLATE", absent = "", inPlace = TRUE)
        
        ### only the filename should be left at this point                          
        dataSec <- stripBlanks( dataSec )
        # fileName <- .getFilePath( dataaSec  , modFile, debug=debug)
        # TODO: The following line might not be correct
        fileName <- .getFile(dataSec)
        
        c( "File" = fileName, "IG" = allIgnore, "ACCEPT" = accept, 
                "REWIND" = rewind, "RECORDS" = records, 
                "TRANSLATE" = translate, "NULL" = null )
    }   
    out <- sapply( x, .extractDataInfo)  
    t(out)
}

