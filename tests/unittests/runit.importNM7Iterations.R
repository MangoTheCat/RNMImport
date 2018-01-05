# $LastChangedDate: $
# $LastChangedBy: $
# $Rev: $
# 
# Author: fgochez
###############################################################################


# this checks that iteration files can be loaded together with a NONMEM run

test.importNM7Iterations <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testDataPath <- file.path(unitTestPath, "testdata/TestDataNM7" )
	
	# test case 1
	
	iter <- RNMImport:::importNm7Iterations(c("TestData1.Ext", ""), noTitles = c("0", "0"), noLabels = c("0", "0"), methods = c("A", "B")
	, path = testDataPath)
	
	expIter <- importNm7Tables("TestData1.Ext", path = testDataPath, type = "ext" )
	
	checkEquals(iter$"Iterative Two Stage", expIter[[1]], check.attributes = FALSE)
	checkEquals(iter$`Stochastic Approximation Expectation-Maximization`, expIter[[2]], check.attributes = FALSE)
	
	# test case 2 : iterations spread across multiple files
	
	iter2 <- RNMImport:::importNm7Iterations(c("TestData1_2.EXT", "extfile2_2.EXT" ), noTitles = c("1","0"), 
			noLabels = c("0","0"), methods = c("A","B"), path = testDataPath )
	expIter1 <- importNm7Tables("TestData1_2.EXT", path = testDataPath, type = "ext", tableTitles = FALSE)
	checkEquals(iter2[[1]], expIter1[[1]], check.attributes = FALSE, msg = " |")
	expIter2 <- importNm7Tables("extfile2_2.EXT", path = testDataPath, type = "ext")
	checkEquals(iter2$"Stochastic Approximation Expectation-Maximization", expIter2[[1]], check.attributes = FALSE )
	
	# test case 3 : only one iterations file, for the final estimation method
	
	iter3 <- RNMImport:::importNm7Iterations(c("", "extfile2_2.EXT" ), noTitles = c("0","1"), 
			noLabels = c("0","0"), methods = c("A","B"), path = testDataPath )

	checkEquals(iter3, list( "Stochastic Approximation Expectation-Maximization" =expIter2[[1]] ), check.attributes = FALSE )

	# test case 4 : repeated file names are ignored
	
	iter4 <- RNMImport:::importNm7Iterations(c("TestData1.EXT", "TestData1.EXT" ), noTitles = c("0","0"), 
			noLabels = c("0","0"), methods = c("A","B"), path = testDataPath )
	checkEquals(iter4$"Iterative Two Stage", expIter[[1]], check.attributes = FALSE, msg = " | correct import on repeated files")
	checkEquals(iter4$`Stochastic Approximation Expectation-Maximization`, expIter[[2]], check.attributes = FALSE, msg = " |correct import on repeated files")
	checkEquals(length(iter4), 2, sg = "| Correct number of iterations imported")
	
}
