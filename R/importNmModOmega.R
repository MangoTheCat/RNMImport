
#' @noRd
#' @export

.extractInformation <- function( x, guessNames = TRUE, rx = "([^[:space:]~]+)$")
{
  ### BLOCK style, indicates a block diagonal specification                                                             
  # retrieve the number of blocks present
  # extract comments	
  comments <- stripBlanks( commentPop( x, inPlace = TRUE )  )
  
  nBlocks <- equalExpressionPop( x, "BLOCK", sep = "[[:space:]]*", removeBrackets = TRUE , 
                                 absent =  NULL , inPlace = TRUE)
  if( !is.null(nBlocks) ){        
    fixed <- logicalPop( x, "FIXE?D?", inPlace = TRUE )
    same <- logicalPop( x, "SAME", inPlace = TRUE) 
    values <- NULL
    values <- if(!same)try( .buildSymMatrix( as.numeric( .readValues(x) ) ) ) 
    out <- list(values = values, block=as.numeric(nBlocks), FIX = fixed, SAME = same, comments = comments)
    if( length( comments)>0 && guessNames )
    {
      guess <- ogrep( rx, comments, filter = "\\1")
      guess <- negGrep( "^[[:digit:]]", guess, value = TRUE ) # name should not start with a digit
      if( !same && ( length(guess) == nrow(out$values) ) ){
        dimnames(out$values) <- rep(list(guess), 2)
      }
    }
    else {
      dimnames(out$values) <- NULL
    }
    out
  }
  else 
  {  ### Independent OMEGAs                                                              

    equalExpressionPop( x, "DIAG", sep = "[[:space:]]*", inPlace = TRUE )                                                
    
    # check for the presence of "FIXED"
    x <- gsub( "[[:space:]]*(FIXE?D?)[[:space:]]*", "FIX", x, ignore.case = TRUE) 
    x <- regexSplit(x, "\\)?[[:space:]]+\\(?")
    x <- gsub( "FIX", " FIX ", x ) 
    fixed <- sapply(x,logicalPop,"FIX", inPlace = TRUE)
    x <- gsub( "[[:space:]]*(FIXE?D?)[[:space:]]*", "", x, ignore.case = TRUE) 
    x <- gsub( "[\\(\\)]", "", x )
    values <- as.numeric( .readValues( x ) )
    out <- data.frame(values=values, FIX = fixed)
    if ( length(comments)  > 0) out$comments <- comments
    if ( length(comments) == 0) out$comments <- rep(NA, nrow(out))

    if( length( comments)>0 && guessNames )
    {
      guess <- ogrep( rx, comments, filter = "\\1")
      guess <- negGrep( "^[[:digit:]]", guess, value = TRUE ) # name should not start with a digit
      if( length(guess) == nrow(out) ){
        row.names(out) <- guess
      }
    }
    else {
      row.names(out) <- NULL
    }
    out
  }
}

#' @noRd
#' @export

.extractInitialInformation <- function( x, guessNames = TRUE, rx = "([^[:space:]~]+)$")
{
  # extract comments	
  comments <- commentPop( x, inPlace = TRUE )  

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
      x <- gsub( "[[:space:]]*(FIXE?D?)[[:space:]]*", "FIX", x, ignore.case = TRUE) 
      x <- regexSplit(x, "\\)?[[:space:]]+\\(?")
      x <- gsub( "FIX", " FIX ", x ) 
      fixed <- sapply(x,logicalPop,"FIX", inPlace = TRUE)
      x <- gsub( "[[:space:]]*(FIXE?D?)[[:space:]]*", "", x, ignore.case = TRUE) 
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


#' @noRd
#' @export

.importNmModOmega <- function(
		txt = NULL, 
		guessNames = TRUE, 
		component = c("OMEGA", "SIGMA"), 		 
		file = NULL
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
	out <- lapply( omegas, .extractInformation, guessNames = guessNames)
	mList <- lapply( omegas, .extractInitialInformation, guessNames = guessNames)
	out$initialMatrix <- blockBind( mList, component, TRUE )
	out
	
}
