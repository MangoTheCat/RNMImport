test.partitionByProblem <- function()
{
	scanFile <- RNMImport:::scanFile
	partitionByProblem <- RNMImport:::partitionByProblem
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testText <- scanFile(file.path(unitTestPath, "testdata/multiprob1.mod"))
	x <- partitionByProblem(testText)
	checkEquals(length(x), 2)
	checkEquals(length(x[[1]]), 26)
	checkEquals(length(x[[2]]), 7)
	checkEquals(x[[2]][1], "$PROB POPULATION DATA WITH PRIOR ON THETA AND OMEGA")
	checkEquals(x[[1]][1], "$PROB READ THE MODEL SPECIFICATION FILE" )
}