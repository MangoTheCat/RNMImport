
test.logging <- function()
{
	checkEquals(availableLogs(), c("stdReport", "detailedReport", "warnings", "highLevelParse", "lowLevelParse"))
	# should throw an exception
	checkException(setLogConnection("fakeLog", NULL), msg = "Testing error-handling")
	
	# try closing logs that are not open for the correct errors
	# this checks against ticket 0001157 
	
	z <- try(closeLogConnection("warnings", FALSE))
	
	 checkEquals(as.character(z),  "Error in closeLogConnection(logName = \"warnings\", allowFail = FALSE) : \n  Unable to close the connection warnings \n\n"
					, msg = " correct error message when trying to close log without sucess" )
	
	z <- try(closeLogConnection("warnings", TRUE))
	# adding a check that this is not an error.  This is not strictly needed because RUnit will report an error if there is one, but is included
	# because it is cleaner.  Also related to issue 1157.
	checkTrue(is.na(z), msg = " | Attempting to close a connection with allowFail = TRUE does not generate an error")
	
	# failure is now allowed without generating an exception, so check if the warning is true
	# TODO: the following looks like it should work, but does not for some reason.  Fix it.
	# checkEquals(names(last.warning)[1],  "Unable to close the connection warnings , but proceeding anyway \n" )
	
	# try to direct a log to a file, output to it, then check	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	setLogFile("stdReport", file.path(unitTestPath, "testdata/testlog.txt"))
	logMessage("test", "stdReport")
	checkEquals("test", scan(file.path(unitTestPath, "testdata/testlog.txt"), what = ""))
	closeLogConnection("stdReport")
	checkTrue(is.na(logConnection("stdReport")))
	file.remove(file.path(unitTestPath, "testdata/testlog.txt"))
	setLogConnection("stdReport", stdout())
}
