
#' Parse $EST statements from a control file and return a list of parsed statements (one element for each $EST) 
#' At the moment, these are just strings, but this will likely change
#' @param txt A vector of strings containing the text of the $EST statements to be parsed 
#' @return A matrix of parsed statements.  Each row of the matrix will correspond to one parsed $EST statement.
#' These will have 4 columns
#' @author Mango Solutions


.importNmModEst <- function(txt )
{
	estSectionText <- section(txt, "EST", oneline = FALSE, stripout = TRUE, glue = TRUE)
	
	# there is a possible bug in NONMEM 7 in its handling of NOTITLE.  It claims that 0 is the default value.
	# However, the default is whatever the last explicitly statement value is.  This necessitates the following 
	# variable
	
	defaultNotitleVal <- "0"
	
	.parseEst <- function(estElement)
	{
		# we wish to determine the values of flags METHOD, FILE, NOTITLE, and NOLABEL 
		
		x <- equalExpressionPop(estElement, "METHOD", shortcut = TRUE, inPlace = FALSE)
		meth <- x$op.out
        if(is.null(meth)) meth='0'
		x <- x$txt
		
		x <- equalExpressionPop(x, "FILE", shortcut = TRUE, inPlace = FALSE)
		fileName <- if(!is.null(x$op.out)) x$op.out else ""
		
		x <- x$txt
		
		x <- equalExpressionPop(x, "NOTITLE", shortcut = TRUE, inPlace = FALSE)
		
		if(!is.null(x$op.out))
		{
			noTitle <- x$op.out
			assign("defaultNotitleVal",x$op.out ,envir = parent.frame(3))
			# defaultNotitleVal <- x$op.out
		}
		else
			noTitle <- defaultNotitleVal

		x <- x$txt
		
		x <- equalExpressionPop(x, "NOLABEL", shortcut = TRUE, inPlace = FALSE)
		noLabel <- if(!is.null(x$op.out)) x$op.out else "0"
		
		remainingText <- x$txt
		
        if(is.null(remainingText) || length(remainingText)<1) remainingText=''
		c("method" = meth, "file" = fileName, "noTitle" = noTitle, "noLabel" = noLabel,
				"remainingText" = remainingText)

	}
	
	parsedEstSections <- sapply(estSectionText, .parseEst)
	t(parsedEstSections)
}
