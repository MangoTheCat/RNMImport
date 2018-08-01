

#' Parses the $PRIOR statement of NONMEM 7 control files.  The parsed statement is turned into a named vector
#' @title Parse PRIOR statement
#' @param txt Control file text containing a $PRIOR statement 
#' @param rx 
#' @param fileName  
#' @return A named vector with entries for the prior : nEta, nThp, nEtp, npExp, nTheta.  There will also be an
#' NWPRI attribute which will be true or false depending on whether or not NWPRI is present
#' @author Mango Solutions
#' @export

.importNmModPrior <- function(
        txt = NULL,       
        rx = "([^~[:space:]]+)$", # TODO: This should be a changeable option          
        fileName = NULL
)
{
    if(is.null(txt)) txt <- scanFile(fileName)
    priorText <- section(txt, "PRIOR", as.list = FALSE, stripout = TRUE )

    #    This is to cope with PRIOR called as part of $SUBR
    if(length(priorText)>0){
        # extract the comments
        comments <- stripBlanks( commentPop( priorText, inPlace = TRUE ) )
        # check that "NWPRI" is present
        nWpri <- pop(priorText, "NWPRI", inPlace = TRUE)
        # replace commas with blanks so that equalExpressionPop can be used correctly
        priorText <- gsub(priorText, pattern = "\\,", replacement = " ")
        
        # extract the individual elements of the form NTHETA=X, NETA=Y, etc.
        
        nTheta <- as.numeric(equalExpressionPop( priorText, "NTHETA", shortcut = TRUE, inPlace = FALSE)$op.out)
        nEta <- as.numeric(equalExpressionPop(priorText, "NETA", shortcut = TRUE, inPlace = FALSE)$op.out)
        
        nThp <- as.numeric(equalExpressionPop(priorText, "NTHP", shortcut = TRUE, inPlace = FALSE)$op.out)
        nEtp <- as.numeric(equalExpressionPop(priorText, "NETP", shortcut = TRUE, inPlace = FALSE)$op.out)
        npExp <- as.numeric(equalExpressionPop(priorText, "NPEXP", shortcut = TRUE, inPlace = FALSE)$op.out)
        
        # res <- as.numeric(c( nTheta = nTheta, nEta = nEta, nThp = nThp, npExp = npExp, nEtp = nEtp ))
        res <- c( nTheta = nTheta, nEta = nEta, nThp = nThp, npExp = npExp, nEtp = nEtp )

        attr(res, "NWPRI") <- nWpri
        return(res)
    } else {
#        Find the subroutine part
        subText <- section(txt, "SUB", as.list = FALSE, stripout = TRUE )
        if(length(subText)==0)
            return(NULL)
        # extract the individual elements of the form NTHETA=X, NETA=Y, etc.
        
#        find the set of THETA, ETA and SIGMA in the PK section
        nTheta <- lapply(txt, function(X){
                    zzz <- gregexpr('(THETA[(])[0-9]{0,3}', X)[[1]]
                    if(any(zzz>0))
                        return(substring(X, zzz, zzz + attr(zzz, 'match.length')))
                    NULL
                }
        )
        nTheta <- length(unlist(nTheta))
        nEta <- lapply(txt, function(X){
                    zzz <- gregexpr('([ *+-/]ETA[(])[0-9]{0,3}', X)[[1]]
                    if(any(zzz>0))
                        return(substring(X, zzz, zzz + attr(zzz, 'match.length')))
                    NULL
                }
        )
        nEta <- length(unlist(nEta))
        
        nThp <- lapply(txt, function(X){
                    zzz <- gregexpr('( NTHP[(])[0-9]{0,3}', X)[[1]]
                    if(any(zzz>0))
                        return(substring(X, zzz, zzz + attr(zzz, 'match.length')))
                    NULL
                }
        )
        nThp <- length(unlist(nThp))
        
        nEtp <- lapply(txt, function(X){
                    zzz <- gregexpr('( NETP[(])[0-9]{0,3}', X)[[1]]
                    if(any(zzz>0))
                        return(substring(X, zzz, zzz + attr(zzz, 'match.length')))
                    NULL
                }
        )
        nEtp <- length(unlist(nEtp))
        
        npExp <- lapply(txt, function(X){
                    zzz <- gregexpr('( NPEXP[(])[0-9]{0,3}', X)[[1]]
                    if(any(zzz>0))
                        return(substring(X, zzz, zzz + attr(zzz, 'match.length')))
                    NULL
                }
        )
        npExp <- length(unlist(npExp))
        
        nWpri <- lapply(txt, function(X){
                    zzz <- gregexpr('( NPEXP[(])[0-9]{0,3}', X)[[1]]
                    if(any(zzz>0))
                        return(substring(X, zzz, zzz + attr(zzz, 'match.length')))
                    NULL
                }
        )
        
        nWpri <- length(unlist(nWpri))
        
        res <- c( nTheta = nTheta, nEta = nEta, nThp = nThp, npExp = npExp, nEtp = nEtp )
        
        attr(res, "NWPRI") <- nWpri
        return(res)
        
    }
    
    
}
