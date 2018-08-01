
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
#' x <- runRNMImportTests(TestResult = "runRNMImportTests_tests")
#' summary(x)
#' }
#'

runRNMImportTests <- function(TestPath = system.file(package="RNMImport", "unittests"), 
		ExcludeFolders = NULL, TestResult = NULL, ResultsType = c("html", "text"))
{
	if(!require("RUnit", quietly = TRUE)) stop("There is no 'RUnit' package!")
	TestPath <- normalizePath(TestPath, winslash = "/", mustWork = TRUE)
	ResultsType <- match.arg(ResultsType)
	if (!exists(".RNMImportTestEnv", envir = .GlobalEnv)) assign(".RNMImportTestEnv", new.env(), envir = .GlobalEnv)
	assign("InternalDataPath", TestPath, envir = .RNMImportTestEnv)
	
	TestFolders <- list.dirs(TestPath, full.names = TRUE, recursive = FALSE)
	TestFolders <- TestFolders[!basename(TestFolders) %in% ExcludeFolders]
	if (length(TestFolders) > 0) {
		TestSuite <- list()
		for (i in seq_along(TestFolders)) {
			TestSuiteName <- paste0("RNMImport Tests - ", basename(TestFolders)[i])
			TestSuite[[i]] <- defineTestSuite(TestSuiteName, dirs = TestFolders[i], testFileRegexp = "^runit\\..+\\.[rR]$") 
		}
	} else {
		TestSuite <- defineTestSuite("RNMImport Tests", dirs = TestPath, testFileRegexp = "^runit\\..+\\.[rR]$")
	}
	
	OUT <- runTestSuite(TestSuite)
	if(!is.null(TestResult)) {
		TestResult <- paste(gsub(paste("\\.", ResultsType, sep = ""), "", 
						TestResult, ignore.case = TRUE), ResultsType, sep = ".")
		if (ResultsType == "html") printHTMLProtocol(OUT, fileName = TestResult)
		if (ResultsType == "text") printTextProtocol(OUT, fileName = TestResult)
	} 
	if (exists(".RNMImportTestEnv", envir = .GlobalEnv)) rm(.RNMImportTestEnv, envir = .GlobalEnv)
	return(OUT)
}



