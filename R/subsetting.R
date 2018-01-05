

# TODO: make this a generic function later

#' Sets the subset of an object
#' @param x Object whose subset should be modified.
#' @param value The subset to attach
#' @return None
#' @author Mango Solutions
#' @export

"dataSubset<-" <- function(x, value)
{
	RNMImportStopifnot(inherits(x, "data.frame") |  inherits(x, "NMProblem"), "x is not of the correct class")
	if(is.null(value))
		return(x)
	assertClass(value, "character")
	attr(x, "dataSubset") <- value
	x
}

#' Adds subset expressions to the subset of an object
#' @param x Object whose subset should be modified.
#' @param value The subset to attach
#' @return None
#' @author Mango Solutions
#' @export

"augmentDataSubset<-" <- function(x, value)
{
	RNMImportStopifnot(inherits(x, "data.frame") |  inherits(x, "NMProblem"), "x is not of the correct class")
	if(is.null(value))
		return(x)
	assertClass(value, "character")
	newSubset <- unique(c(dataSubset(x), value ))
	dataSubset(x) <- newSubset
	x
}

# TODO: take this out of RNMGraphics later

#' @description NONMEM data objects and data frames can have "graphical subsets" associated to them. 
#' These subsets will be applied whenever a graphing function is called on the object. 
#' This functionality is intended to facilitate the plotting of "standard" subsets, i.e. subsets without dosing information. 
#' @details Subsets assigned to an object are obtained using \code{graphSubset}. Subsets will be a set a vector of strings 
#' which are converted to R expressions and then executed. They can be assigned using \code{graphSubset<-}.
#' @name Graphical Subsetting
#' @aliases graphSubset graphSubset<-
#' @title Change and Retrieve Graphical Subsets
#' @param x An object of class \code{NMRun}, \code{NMProblem} or \code{data.frame}.
#' @param value A vector of (character) subsets.
#' @return For \code{graphSubset}, a character vector representing the subsets to be applied to \code{x} before plotting. 
#' For \code{graphSubset<-}, the updated object, i.e. with additional or removed subsets.
#' @author Mango Solutions
#' @keywords environment
#' @export
#' @examples 
#' \dontrun{
#' x <- importNm("TestData1.ctl", "TestData1.lst", 
#'   path  = system.file(package = "RNMImport", "unittests/testdata/TestRun")) 
#' graphSubset(x) <- c("MDV != 1", "EVID == 0", "AMT <= 0") 
#' graphSubset(x)
#' }
#' 
#' @export

"graphSubset<-" <- function(x, value)
{
	attr(x, "graphSubset") <- value
	x
}

#' @name Graphical Subsetting
#' @aliases graphSubset graphSubset<-

graphSubset <- function(x)
{
	attr(x, "graphSubset")
}

#' Retrieves the data subset associated with a particular object
#' @param x Object (data.frame or NMProblem)
#' @return Character vector of expressions
#' @author Mango Solutions
#' @export

dataSubset <- function(x)
{
	attr(x, "dataSubset")
}

#' Applies a set of subset expressions to a data.frame
#' @param obj A data.frame to be subsetted
#' @param sub Character vector of subsets to apply
#' @param verboseWarnings [L, 1] Logical flag.  If TRUE, will emit a warning whenever a subset statement fails
#' @return subsetted data.frame, with subsets in sub applied
#' @author Mango Solutions
#' @export

applyDataSubset <- function(obj, sub = NULL, verboseWarnings = FALSE)
{
	assertClass(obj, "data.frame")
	if(is.null(sub))
		return(obj)
	
	assertClass(sub, "character")
	for(x in sub)
	{
		res <- try(subset(obj, eval(parse(text = x)) ), silent = TRUE)
		if(!inherits(res, "try-error")) obj <- res
		else if(verboseWarnings){
			RNMImportWarning(paste("Unable to apply subset:",x,"\n" ), call = match.call())
		}
		
	}
	return(obj)	
}
