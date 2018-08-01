
.extractInformation <- function( x, guessNames = TRUE, rx = "([^[:space:]~]+)$")
{
	# extract comments	
	comments <- commentPop( x, inPlace = TRUE )  
	# check for the presence of "FIXED"
	fixed <- logicalPop( x, "FIXE?D?", inPlace = TRUE) 
		
	### SAME STYLE                                                              
	
	if(logicalPop( x, "SAME", inPlace = TRUE) )  
		out <- matrix("SAME", 1, 1)
	else
	{ ### BLOCK style, indicates a block diagonal specification                                                             
		# retrieve the number of blocks present
		nBlocks <- equalExpressionPop( x, "BLOCK", sep = "[[:space:]]*", removeBrackets = TRUE, 
					absent =  NULL, inPlace = TRUE)
		if( !is.null(nBlocks)){        
			out <- try( .buildSymMatrix( as.numeric( .readValues(x) ) ) )
		} 
		else 
		{  ### DIAG style                                                              
				equalExpressionPop( x, "DIAG", sep = "[[:space:]]*", inPlace = TRUE )                                                
				x <- gsub( "[\\(\\)]", "", x )
				out <- as.numeric( .readValues( x ) )
				out <- if( length(out)==1) as.matrix(out) else diag(out)
		}
		
	}
	if( !is.null( comments) && guessNames )
	{
		guess <- ogrep( rx, comments, filter = "\\1")
		guess <- negGrep( "^[[:digit:]]", guess, value = TRUE ) # name should not start with a digit
		if( length(guess) == nrow(out) ){
			dimnames(out) <- rep(list(guess), 2)
		}
	}
	out
}


.importNmModOmega <- function(
		txt = NULL, 
		guessNames = TRUE, 
		component = c("OMEGA", "SIGMA"), 		 
		file = NULL
		#rx = "([^[:space:]~]+)$" 
		)
{	
	if(!is.null(file))
		txt <- scanFile(file)
	component <- match.arg(component)
	.extract <- length(grep( sprintf("\\$", component), txt))
	if( is.null( txt)) return(NULL)
	
	### import the OMEGA declarations                                             
	omegas <- if(.extract) section( txt, component, "", stripout = TRUE ) else txt
	
	### each $OMEGA is a separate block
	# this is somewhat complex because omegas can be specified in different ways
	mList <- lapply( omegas, .extractInformation, guessNames = guessNames)
	
	### structure the output in one single matrix                                 
	out <- blockBind( mList, component, TRUE )
	out
	
}
