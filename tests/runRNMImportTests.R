
stopifnot(require(RNMImport, quietly = TRUE))

.RNMImportTestEnv <- new.env()

#' Run unit tests.
#'
#' Run the unit tests by RUnit package, and generate a html or text report. 
#' @title Run unit tests.
#' @param TestPath Path of the folder which contains the test scripts.
#' @param ExcludeFolders The folders are not tested.
#' @param TestResult Name of the report file.
#' @param ResultsType 'html' or 'text'.
#' @return The results of function \code{\link{[RUnit]runTestSuite}}. 
#' @author Mango Solutions
#' @keywords debugging
#' @examples \dontrun{
#' ## Run this from the /tests folder
#' source("runRNMImportTests_tests")
#' }
#' @noRd

runRNMImportTests <- function(TestPath = "./unittests",
		ExcludeFolders = NULL, TestResult = NULL, ResultsType = c("html", "text"))
{
	if(!require("RUnit", quietly = TRUE)) stop("There is no 'RUnit' package!")
	TestPath <- normalizePath(TestPath, winslash = "/", mustWork = TRUE)
	ResultsType <- match.arg(ResultsType)
	assign("TestPath", TestPath, envir = .RNMImportTestEnv)
	
	TestFolders <- TestPath
	TestFolders <- TestFolders[!basename(TestFolders) %in% ExcludeFolders]
	if (length(TestFolders) > 0) {
		TestSuite <- list()
		for (i in seq_along(TestFolders)) {
			TestSuiteName <- paste0("RNMImport Tests - ", basename(TestFolders)[i])
			TestSuite[[i]] <- RUnit::defineTestSuite(TestSuiteName, dirs = TestFolders[i], testFileRegexp = "^runit\\..+\\.[rR]$") 
		}
	} else {
		TestSuite <- RUnit::defineTestSuite("RNMImport Tests", dirs = TestPath, testFileRegexp = "^runit\\..+\\.[rR]$")
	}
	
	OUT <- RUnit::runTestSuite(TestSuite[[1]])
	if(!is.null(TestResult)) {
		TestResult <- paste(gsub(paste("\\.", ResultsType, sep = ""), "", 
						TestResult, ignore.case = TRUE), ResultsType, sep = ".")
		if (ResultsType == "html") RUnit::printHTMLProtocol(OUT, fileName = TestResult)
		if (ResultsType == "text") RUnit::printTextProtocol(OUT, fileName = TestResult)
	} 
	return(OUT)
}


OUT <- runRNMImportTests()
summary(OUT)
