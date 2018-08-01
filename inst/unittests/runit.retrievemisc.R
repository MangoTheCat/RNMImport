
# tests retrieval of objective function
# getObjective

test.getObjective <- function()
{

	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun") )
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	# NMBasicModel

	checkEquals(getObjective(getProblem(run1), addMinInfo = FALSE), 3228.988 ) 
	
	obj <- getObjective(run1, addMinInfo = TRUE)
	target <- 3228.988
	attr(target, "minInfo") <- c("minResult" = "SUCCESSFUL", "numEval" = "143", 
			"numSigDigits" = "3.5")
	checkEquals(obj, target)
	
	# NMSimModel
	
	run2 <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	prob2 <- getProblem(run2)
	checkEquals(getObjective(run2, subProblems = 1:5), getObjective(prob2, subProblems = 1:5))
	checkEquals(getObjective(prob2, subProblems = c(2, 4)), 
			structure(c(3575.252, 3606.526), .Names = c("sim2","sim4")),
			msg = " |objective functions for simulation problem are correct")
	
	# NMBasicModelNM7
	
	run3 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	prob3 <- getProblem(run3)
	
	checkEquals(getObjective(run3, method = 1:2), getObjective(prob3, method = 1:2), msg = " |getobjective the same on run and NMBasicModelNM7" )
	
	objTarget <- c(3335.250, 2339.093)
	attr(objTarget, "minInfo") <- c("OPTIMIZATION COMPLETED", "STOCHASTIC PORTION COMPLETED")
	checkEquals(getObjective(run3, method = 1:2), objTarget)
	checkEquals(getObjective(prob3, method = 1, addMinInfo = FALSE), 3335.250 )
	checkEquals(getObjective(prob3, method = 2, addMinInfo = FALSE), 2339.093 )
	
	removeNmPath("internalunit")
	
	
}

# test get estimate cov

test.getEstimateCov <- function()
{

	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1notab.ctl", path = file.path(unitTestPath, "testdata/TestRunNoTab" ))
	prob1 <- getProblem(run1)
	
	checkEquals(getEstimateCov(run1, corMatrix = TRUE) ,getEstimateCov(run1, corMatrix = TRUE))
	# expected covariance matrices
	expCovMat <- structure(c(0.927, 2.16, 0.0347, 0.00617, 0, 0, 0.00252, 0, 0.0471, 
					0.00176, 2.16, 12.7, 0.18, 0.00892, 0, 0, 0.00903, 0, 0.39, 0.000474, 
					0.0347, 0.18, 0.0474, -0.000661, 0, 0, -0.000648, 0, 0.0657, 
					0.000168, 0.00617, 0.00892, -0.000661, 0.000571, 0, 0, 0.000373, 
					0, -0.000778, 1.59e-05, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
					0, 0, 0, 0, 0, 0, 0, 0.00252, 0.00903, -0.000648, 0.000373, 0, 
					0, 0.000486, 0, -0.000812, 6.95e-06, 0, 0, 0, 0, 0, 0, 0, 0, 
					0, 0, 0.0471, 0.39, 0.0657, -0.000778, 0, 0, -0.000812, 0, 0.119, 
					0.000239, 0.00176, 0.000474, 0.000168, 1.59e-05, 0, 0, 6.95e-06, 
					0, 0.000239, 1.04e-05), .Dim = c(10L, 10L), .Dimnames = list(
					c("TH1", "TH2", "TH3", "OM11", "OM12", "OM13", "OM22", "OM23", 
							"OM33", "SG11"), c("TH1", "TH2", "TH3", "OM11", "OM12", "OM13", 
							"OM22", "OM23", "OM33", "SG11")))
	
	expCorMat <- structure(c(1, 0.629, 0.166, 0.268, 0, 0, 0.119, 0, 0.142, 0.567, 
					0.629, 1, 0.232, 0.105, 0, 0, 0.115, 0, 0.317, 0.0413, 0.166, 
					0.232, 1, -0.127, 0, 0, -0.135, 0, 0.874, 0.24, 0.268, 0.105, 
					-0.127, 1, 0, 0, 0.708, 0, -0.0943, 0.206, 0, 0, 0, 0, 0, 0, 
					0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.119, 0.115, -0.135, 
					0.708, 0, 0, 1, 0, -0.107, 0.0978, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
					0, 0.142, 0.317, 0.874, -0.0943, 0, 0, -0.107, 0, 1, 0.215, 0.567, 
					0.0413, 0.24, 0.206, 0, 0, 0.0978, 0, 0.215, 1), .Dim = c(10L, 
					10L), .Dimnames = list(c("TH1", "TH2", "TH3", "OM11", "OM12", 
							"OM13", "OM22", "OM23", "OM33", "SG11"), c("TH1", "TH2", "TH3", 
							"OM11", "OM12", "OM13", "OM22", "OM23", "OM33", "SG11")))
	
	checkEquals(getEstimateCov(prob1), expCovMat, msg = " |covariance matrix as expected")
	test1 <- getEstimateCov(run1, corMatrix = TRUE)
	
	checkEquals(test1, list("covariance" = expCovMat, "correlation" = expCorMat),
			msg = " | extracting with both")

	# check for appropriate error handling
	
	run2 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	checkTrue(is.null(getEstimateCov(run2)), msg = " |NULL returned when no parameter covariance matrix found" )
	
	############## # now NONMEM 7 run
	run3 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	prob3 <- getProblem(run3)
	
	# for method 2, should be NULL
	checkTrue( is.null(getEstimateCov(run3, method = 2) ), msg = " |NULL returned when no parameter covariance matrix found")
	
	expCov2 <- structure(c(1.53, -6.69, 0.00102, 0.00653, 0, 0, -0.00655, 0, 
					-0.0794, -0.00023, -6.69, 59.4, -0.0543, 0.0591, 0, 0, -0.14, 
					0, 0.188, 0.00104, 0.00102, -0.0543, 0.0162, -0.000882, 0, 0, 
					0.000434, 0, 0.00117, 2.73e-05, 0.00653, 0.0591, -0.000882, 0.00182, 
					0, 0, -0.00151, 0, -0.00198, -1.23e-06, 0, 0, 0, 0, 0, 0, 0, 
					0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.00655, -0.14, 0.000434, 
					-0.00151, 0, 0, 0.00233, 0, 0.00143, 2.41e-07, 0, 0, 0, 0, 0, 
					0, 0, 0, 0, 0, -0.0794, 0.188, 0.00117, -0.00198, 0, 0, 0.00143, 
					0, 0.0403, -4.54e-06, -0.00023, 0.00104, 2.73e-05, -1.23e-06, 
					0, 0, 2.41e-07, 0, -4.54e-06, 5.56e-07), .Dim = c(10L, 10L), .Dimnames = list(
					c("TH1", "TH2", "TH3", "OM11", "OM12", "OM13", "OM22", "OM23", 
							"OM33", "SG11"), c("TH1", "TH2", "TH3", "OM11", "OM12", "OM13", 
							"OM22", "OM23", "OM33", "SG11")))
	
	expCor2 <- structure(c(1, -0.702, 0.0065, 0.124, 0, 0, -0.11, 0, -0.32, 
					-0.25, -0.702, 1, -0.0553, 0.18, 0, 0, -0.377, 0, 0.122, 0.181, 
					0.0065, -0.0553, 1, -0.162, 0, 0, 0.0705, 0, 0.0457, 0.288, 0.124, 
					0.18, -0.162, 1, 0, 0, -0.733, 0, -0.232, -0.0388, 0, 0, 0, 0, 
					0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.11, -0.377, 
					0.0705, -0.733, 0, 0, 1, 0, 0.148, 0.0067, 0, 0, 0, 0, 0, 0, 
					0, 0, 0, 0, -0.32, 0.122, 0.0457, -0.232, 0, 0, 0.148, 0, 1, 
					-0.0303, -0.25, 0.181, 0.288, -0.0388, 0, 0, 0.0067, 0, -0.0303, 
					1), .Dim = c(10L, 10L), .Dimnames = list(c("TH1", "TH2", "TH3", 
							"OM11", "OM12", "OM13", "OM22", "OM23", "OM33", "SG11"), c("TH1", 
							"TH2", "TH3", "OM11", "OM12", "OM13", "OM22", "OM23", "OM33", 
							"SG11"))) 
	
	cov2Test <- getEstimateCov( prob3, method = 1 )
	
	checkEquals(cov2Test, expCov2, msg = " | covariance matrix OK")
	
	checkEquals(getEstimateCov(prob3, corMatrix = TRUE), 
			list("covariance" = expCov2, "correlation" = expCor2),
			msg = " |retrieving both at the same time works")
}

# tests the following functions:
# getNmVersion
#

test.getNmVersion <- function()
{

	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1notab.ctl", path = file.path(unitTestPath, "testdata/TestRunNoTab" ))
	run2 <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	
	checkEquals( getNmVersion(run1), c(major = "VI", "minor" = "2" ) , " version of run1 is correct")
	checkEquals( getNmVersion(run2), c(major = "VI", "minor" = "1" ) , " version of run2 is correct")
	prob1 <- getProblem(run1)
	prob2 <- getProblem(run2)
	checkEquals( getNmVersion(prob1), c(major = "VI", "minor" = "2" ) , " version of run1 problem is correct")
	checkEquals( getNmVersion(prob2), c(major = "VI", "minor" = "1" ) , " version of run2 problem is correct")
	
}

# test the following functions:
# getMethodNames

test.getMethodNames <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	prob1 <- getProblem(run1)
	
	# check that getMethodNames works in the same way for runs as for problems
	
	checkEquals(getMethodNames(run1, what = "control"), getMethodNames(prob1, what = "control"))
	checkEquals(getMethodNames(run1, what = "report"), getMethodNames(prob1, what = "report"))
	
	checkEquals(getMethodNames(prob1, what = "control"),c("ITS","SAEM"), msg = " |control stream captured correctly" )
	checkEquals(getMethodNames(prob1, what = "report"),c( "Iterative Two Stage",  "Stochastic Approximation Expectation-Maximization"), 
			msg = " |report stream captured correctly" )
	
}

# tests the following functions:
# getFileinfo

test.getFileinfo <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	fInfoTest <- getFileinfo(run1) 
	if (.Platform$OS.type == "windows") {
		colnametest <- c("size", "mode", "mtime", "ctime", "atime", "exe", "fileName")
	} else {
		colnametest <- c("size", "mode", "mtime", "ctime", "atime", "uid", "gid", "uname", "grname", "fileName")
	}
	checkEquals(colnames(fInfoTest), colnametest, msg = " |correct columns present")
	checkEquals(rownames(fInfoTest), c("controlFile", "reportFile" ))
	
	checkEquals(fInfoTest$size, c(941, 7820))
	# checkEquals(fInfoTest$mtime, structure(c(1231859719, 1231859719), class = c("POSIXt", "POSIXct")))
	
	checkEquals(tolower(basename(fInfoTest["controlFile","fileName"])), "testdata1.ctl", msg = " | correct control file name" )
	
	checkEquals(tolower(basename(fInfoTest["reportFile","fileName"])),"testdata1.lst" , msg = " | correct report file name" )
}

# tests the getSimInfo function on a NMSimMode object

test.getSimInfo <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	simInfo <- getSimInfo(run)
	rawInfo <- attr(simInfo, "rawStatement")
	attr(simInfo, "rawStatement") <- NULL
	checkEquals(rawInfo, "(20050213) SUBPROBLEMS=5", msg = " | Correct raw statement extracted")
	checkEquals(simInfo, c("numSimulations" = 5, "seed1" = 20050213, "seed2" = NA), msg = " | Seeds ")
	
	prob <- getProblem(run)
	
	checkEquals(getSimInfo(prob, addRawInfo = FALSE), c("numSimulations" = 5, "seed1" = 20050213, "seed2" = NA) )
	 
}