


#' Extracts estimation method names from a run or problem that used NONMEM 7.  These
#' can be the names used in the control file, or the report file
#' @param obj NMRun, NMBasicModelNM7, or NMSimModelNM7
#' @param what The string "report" or "control" - selects which file should be used
#' for the method names
#' @param problemNum Problem number in run
#' @title Get method names
#' @return A character vector of method names used
#' @note If what = "control", the names are taken from the $EST control sections
#' @author Mango Solutions
#' @export
#' @examples
#' \dontrun{
#'      x <- importNm("TestData1.ctl", "TestData1.lst", path  = "testing/testdata/TestDataNM7")
#'      print(getMethodNames(x))
#' }
#' @keywords methods utilities

getMethodNames <- function(obj, what = c("report", "control" ), problemNum = 1)
{
    RNMImportStop("This function is not implemented for objects of this class")
}

setGeneric("getMethodNames")

getMethodNames.NMRun <- function(obj, what = c("report", "control" ), problemNum = 1)
{
    what <- match.arg(what)
    getMethodNames( getProblem(obj, problemNumber = problemNum), what = what )
}

setMethod("getMethodNames", signature(obj = "NMRun"), getMethodNames.NMRun )

getMethodNames.NMProblemNM7 <- function(obj, what = c("report", "control"), problemNum = 1)
{
    what <- match.arg(what)
    if(what == "report")
        obj@methodNames
    else
        unname(obj@methodInfo[,"method"])
}

setMethod("getMethodNames", signature(obj = "NMBasicModelNM7"), getMethodNames.NMProblemNM7)
setMethod("getMethodNames", signature(obj = "NMSimModelNM7"), getMethodNames.NMProblemNM7)
