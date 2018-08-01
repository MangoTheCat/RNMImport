
#' Tries to force columns in a data.frame that are numeric but have ended up stored as strings back to numerics again
#' @param myData Input data frame
#' @param missToZero replaces numerics
#' @title Force columns that should be numeric to numeric
#' @return The data.frame with all columns that should be numeric, as numeric
#' @author Mango Solutions
#' @note Based on RNONMEM 1 code by R Pugh, J James, and R Francois


.importDataNumeric <- function(myData, missToZero = TRUE)
{
	rNames <- row.names(myData)
	myData <- data.frame(myData)
	# get classes of columns
	myClasses <- sapply(myData, base::class)
	# detect presence of any character or column factors
	
	isFactororChar <- myClasses == "character" | myClasses == "factor"
	
	if(any(isFactororChar)) 
	{
		# cycle through factor and
		for(myVar in names(myData)[isFactororChar]) 
		{  
			myData[[myVar]] <- as.character(myData[[myVar]])
			### Strip starting and ending spaces
			myData[[myVar]] <- stripBlanks( myData[[myVar]], trim = FALSE,  shrink = FALSE, remove.empty = FALSE )
			
			### s/dots/NA/
			# replace periods with "NA"
            anyDot = which(myData[[myVar]] == ".") 
            if (length(anyDot)>0)	myData[ anyDot, myVar] <- NA
			
			### Find character or date/time data
			isDate <- length( grep( "^[0-9]{1,4}[\\/-]", myData[[myVar]]) ) > 0
			if(!isDate) 
			{  # Not a date column but could be general character column
				# check for the presence of numeric data in this column, coercing the entire column to numeric if any is detected
				notNum <- setdiff( which(complete.cases(myData[[myVar]])) , grep( "[[:alpha:]:/\\_]"   ,   myData[, myVar ] )  )
				if(length(notNum)>0){
					myData[[myVar]] <- switch( class(myData[[myVar]]), 
							"character" = as.numeric( myData[[myVar]] ), 
							"factor"    = as.numeric( as.character( myData[[myVar]])) )
				}
			}
		}
	}
	
	### s/NA/0/
	# If missToZero is TRUE, set any NA in the numerical columns to 0
	if(missToZero){
		myClasses <- sapply(myData, class)
		isFactororChar <- myClasses == "character" | myClasses == "factor"
		if( any(!isFactororChar)) {
			for(myVar in names(myData)[!isFactororChar]) {
				myData[is.na(myData[[myVar]]), myVar] <- 0
			}
		}
	}
	
	row.names(myData) <- rNames
	myData
	
}

