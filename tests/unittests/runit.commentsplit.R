test.commentSplit <- function()
{
	commentSplit <- RNMImport:::commentSplit
	scanFile <- RNMImport:::scanFile
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	
	testText <- scanFile(file.path(unitTestPath, "testdata/control3.con"))
	x <- commentSplit(testText)
	checkEquals(length(x), 2)
	checkEquals(c(length(x[[1]]), length(x[[2]])), c(16, 16))
	checkTrue(x$comments[1] == "control3 example from NONMEM guide part VIII")
	
	checkTrue(all(x$comments[-1] == ""))
	checkEquals(x$text[11], "$ERROR")
	checkTrue(all(x$text[-1] != ""))
	
	testText <- scanFile(file.path(unitTestPath, "testdata/control4.con"))
	x <- commentSplit(testText)
	blanks <- c( 1, 6, 7, 8, 9,10)
	checkTrue(all(x$text[blanks] == ""))
	checkTrue(all(x$text[-c(blanks)] != ""))

	checkTrue(all(x$comments[blanks] != ""))
	checkTrue(all(x$comments[-c(blanks, 22:24)] == ""))
	# TODO: add more and better tests
	
}