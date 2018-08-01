# DOSE=AMT TIME CP=DV

test.readNmData <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testDir <- file.path(unitTestPath, "testdata")
	x <- readNmData(file.path(testDir, "data3"))
	checkEquals(dim(x), c(11, 3))
	y <- colMeans(x, na.rm = TRUE)
	checkEquals(y, c(320.0, 5.869091, 5.305), tol = 1e-06, checkNames = FALSE)
	# 320.000000   5.869091   5.305000
	
	x.2 <- readNmData(file.path(testDir, "data3.dat"), ignore = "I")
	checkEquals(dim(x.2), c(9, 3))
	checkEquals(names(x.2), c("AMT", "TIME", "DV"))
	checkEquals(x.2$AMT, x[ -c(8, 11),1])
	# readNmData will no longer accept "code" in ignore statements
	# x.3 <- readNmData(file.path(testDir, "data3"), 
	# 		ignore = "(V2.EQ.12)")
	
	x.3 <-  readNmData(file.path(testDir, "data3"))
	RNMImport:::.readNmData.nmSubset(x.3, nmCode = "(V2.EQ.12)", method = "ignore" )
	checkEquals(x[-10,], x.3 )
	
	# check that ignore = "@" is handled correctly  - it should cause the entire header to be ignored
	# along with other lines beginning with a letter
	
	x.4 <- as.matrix(readNmData(file.path(testDir, "data3.dat"), ignore = "@"))
	rownames(x.4) <- NULL
	checkEquals(colnames(x.4), c("V1", "V2", "V3"), msg = "| column names are ignored with IGNORE=@")
	checkEquals(unname(x.4), unname(as.matrix(x.2)), msg = " |IGNORE=@ should ignore rows beginning with I, and hence should be equal to second test")
	
	
	
}
