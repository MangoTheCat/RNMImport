# $Rev: 12836 $
# $LastChangedDate: 2009-11-01 17:44:04 +0000 (Sun, 01 Nov 2009) $
# $LastChangedBy: fgochez $
###############################################################################
#

test.applyDataSubset <- function()
{
	# check whether or not the default subset is applied correctly
	 
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testData <- read.csv(file.path(unitTestPath, "testdata/subsettest.csv"))
	test1 <- applyDataSubset( testData, defaultDataSubset() )

	removedRows <- c(1,2,9,10,11,19,20)
	checkEquals(test1, testData[ - removedRows,])
	
	# check subset as a single string, rather than a vector.
	# this should give the same result
	test2 <- applyDataSubset(testData, "(MDV != 1) & (EVID == 0) & (AMT <= 0)")
	checkEquals(test1, test2, " |Same results retrieved 2 ways|")
	# NULL subset should give no problems
	
	test3 <- applyDataSubset(testData, NULL)
	checkEquals( testData, test3, msg = " |NULL subset allowed but ignored|" )
	# check that bad subsets will not stop the good subsets from being applied
	test4 <- applyDataSubset( testData, c("FOO == 1", "MDV != 1") )
	test5 <- applyDataSubset(testData, c("aSD }{" , "MDV != 1" ))
	
	checkEquals(test4, testData[ -c(1, 10, 20), ] )
	checkEquals(test4, test5)
	
}

# tests function to update data sets only 

test.changeDataSubset <- function()
{
	STANDARDSUBSET <- c("MDV != 1", "EVID == 0", "AMT <= 0")
	# import a test run
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testRun <- importNm(conFile = "TestData1notab.ctl", reportFile = "TestData1notab.lst", 
			path =  file.path(unitTestPath, "testdata/TestRunNoTab"))
	testProblem <- getProblem(testRun)
	
	# check loading of default
	checkEquals(dataSubset(testProblem), STANDARDSUBSET, msg = " |Default subset has been imported correctly|")
	
	# check that dataSubset and augmentDataSubset work
	dataSubset(testProblem) <- "MDV != 1"
	checkEquals(dataSubset(testProblem), "MDV != 1", msg = " |Subset modified correctly|")
	augmentDataSubset(testProblem) <- "EVID == 0"
	checkTrue(setequal(dataSubset(testProblem), c("MDV != 1", "EVID == 0")), msg = " | Subset updated correctly|")

}

# test error checking for subset updating functions

test.changeDataSubsetErrorHandling <- function()
{
	# check type checking
	checkException( dataSubset(1:10) <- "MDV != 1" )
	checkException( augmentDataSubset(1:10) <- "MDV != 1" )
	checkException( dataSubset( mtcars ) <- 10 )
	checkException( augmentDataSubset( mtcars ) <- 10 )
}