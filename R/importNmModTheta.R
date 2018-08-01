

#' Extracts the "theta" declaration information from the text of a control file
#' @param txt [C,+] - Vector of strings in which to extract THETAs
#' @param guessNames [L,1] - Try to deduce the names of the thetas (CL, KV, etc.) 
#' @param rx Regular expression used for detecting names of thetas
#' @param fileName 
#' @title Parse $THETA statement
#' @return A matrix with one row for each "theta" and 3 columns : 1 for the lower value bounds.
#' one for the initial estimate, and one for the upper bound
#' @author Mango Solutions
#' 

# note: Based on code by R Francois, J James and R Pugh

.importNmModTheta <- function(
		txt = NULL,       
		guessNames = TRUE,
		rx = "([^~[:space:]]+)$",  		 
		fileName = NULL
)
{					
	if(is.null(txt))
		txt <- scanFile(fileName)
	# check if THETA is present in the text, otherwise assume that we are dealing with the text after $THETA
	# The section may also be specified as $THTA, hence the regular expression used below
	.extract <- length(grep("\\$TH(E){0,1}TA", toupper(txt))) > 0
	
	### import the THETA declarations                                             
	
	thetaLines <- if(.extract) section( txt, "TH(E){0,1}TA", "", as.list = FALSE, stripout = TRUE) else txt 
	# extract the comments
	comments   <- stripBlanks( commentPop( thetaLines, inPlace = TRUE ) )  
	# remove space before FIXED
	thetaLines <- gsub( "[[:space:]]*(FIXE?D?)[[:space:]]*", "FIX", thetaLines, ignore.case = TRUE) 
	thetaLines <- .rmSpaceInBrackets( thetaLines )                   # remove spaces inside brackets
	thetaLines <- stripBlanks( thetaLines )
	
	# now we must check for declarations of the form (A B C)
	# if we find them, we will replace them with (A,B,C)
	
	# note that the "extended" parameter was deprecated as of R 2.12.x, so we check the version of R being used to control for this	
	# RVersion: list storing information about version of R in use
	RVersion <- R.Version()
	
	if(as.numeric(RVersion$major)>2 || (as.numeric(RVersion$minor) >= 12 && as.numeric(RVersion$major) == 2))
	{
		thetaLines <- gsub(x = thetaLines, "\\(([-]{0,1}\\d+(?:\\.\\d+)?)\\s+([-]{0,1}\\d+(?:\\.\\d+)?)\\s+([-]{0,1}\\d+(?:\\.\\d+)?)\\)",
				replacement = "(\\1,\\2,\\3)", perl = TRUE)
	}
	else
	{
		thetaLines <- gsub(x = thetaLines, "\\(([-]{0,1}\\d+(?:\\.\\d+)?)\\s+([-]{0,1}\\d+(?:\\.\\d+)?)\\s+([-]{0,1}\\d+(?:\\.\\d+)?)\\)",
				replacement = "(\\1,\\2,\\3)", perl = TRUE, extended = TRUE)
	}
	thetaLines <- regexSplit(thetaLines, "\\)?[[:space:]]+\\(?")
	# add additional spaces around "FIX"
	
	thetaLines <- gsub( "FIX", " FIX ", thetaLines )  
	# initialize the output matrix
	out <- matrix( NA, ncol = 3, nrow = length( thetaLines ), 
			dimnames = list( sprintf("THETA%d", 1:length(thetaLines)), 
					c("Lower", "Est", "Upper")) )
	# now iterate through all of the lines of input
	for( i in seq(along=thetaLines) ){
		th <- thetaLines[i]
		th <- killRegex(th , "\\).*")
		### remove options                                                          
		# TODO: use these things
		noabortfirst <- logicalPop( th, "NOABORTFIRST", inPlace = TRUE )
		abort        <- ynPop( th, "ABORT", inPlace = TRUE )
		# using shortcuts because NUMBERPOINTS may be given as on of:
		# NUMBERPTS, NUMPOINTS, NUMPTS
		numberpoints <- equalExpressionPop( th, "NUM", shortcut = TRUE, inPlace = TRUE)
		
		### import the FIXED  option                                                
		fixed <- logicalPop( th, "FIX", inPlace = TRUE)

		### guess the kind of theta line                                            
		th <- gsub( "[\\(\\)]", "", th) # remove the brackets       
		th <- gsub( "inf", "Inf", th, ignore.case = TRUE ) # tidy up Inf
		
		gx.out <- gregexpr( "," , th )[[1]]
		
		if( length(gx.out)==1 )
		{
			if( gx.out == -1)
			{ ### first kind of theta line: init [FIXED]             
				out[i,2] <- as.numeric(th)
				if(fixed)
				{
					out[i,c(1,3)] <- out[i,2]
				} else
				{
					out[i,1] <- -Inf
					out[i,3] <- Inf
				}
			} 
			else 
			{           ### one comma only, assuming: lower,est,INF            
				lower <- as.numeric( sub( "^([^,]*),.*", "\\1"    , th)    )   # before the first comma
				est   <- as.numeric( sub( "^[^,]*,([^,]*)", "\\1" , th)    )   # between comma 1 and 2
				out[i,1] <- lower
				out[i,2] <- est
				out[i,3] <- Inf
			}
		} 
		else if( length(gx.out == 2) )
		{ 
			### other form, reading: lower, est, upper             
			lower <- as.numeric( sub( "^([^,]*),.*", "\\1"         , th ) )   # before the first comma
			est   <- as.numeric( sub( "^[^,]*,([^,]*),.*", "\\1"   , th ) )   # between comma 1 and 2
			upper <- as.numeric( sub( "^[^,]*,[^,]*,([^,]*)", "\\1", th ) )   # after comma 2
			out[i,1] <- lower
			out[i,2] <- est
			out[i,3] <- upper
		}
	}  
	if( guessNames && length(comments) == nrow(out)  )
	{
		rx.out <- regexpr( rx, comments )
		trythat <- ogrep( rx, comments[rx.out!=-1], filter = "\\1" ) 
		alright <- which( regexpr("[\\(\\)]", trythat) == -1 )
		rownames(out)[rx.out!=-1][alright] <- trythat[alright]
	}
	out 	
}
