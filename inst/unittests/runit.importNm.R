# SVN revision: $Rev: 115297 $
# Date of last change: $LastChangedDate: 2014-07-22 09:21:30 +0100 (Tue, 22 Jul 2014) $
# Last changed by: $LastChangedBy: jli@MANGO.LOCAL $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

##### 
# SYSTEM
# runit.importNm.R
# Tests importNm, along with getThetas, getOmegas and getSigmas
# Note that these functions cannot really be tested seperately, so there is overlap with runit.retrieveparameters.R


# simple test case on some local test runs, data is included in RNMImport/inst/unittests/testdata
# tests NMBasicModel import

test.importNm.Basic <- function()
{
	# start with the test run
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun") )
	
	# import the basic test run
	
	run1 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
			path = "(internalunit)")
	# TODO: abstract away all of these access to class slots for improved encapsulation
	# tests that the correct file names have been retrieved
	
	checkEquals( basename(run1@controlFileInfo$fileName), "testdata1.ctl", msg = " |correct control file name imported" )
	checkEquals( basename(run1@reportFileInfo$fileName), "testdata1.lst", msg = " |correct list file name imported") 
	
	# test that the the number of problems is correct
	
	checkEquals(run1@numProblems, 1, " |run contains the correct number of problems")
	
	# test import of control file and report file text by checking that the correct number of lines have been imported
	
	checkEquals(length(run1@reportText), 166 , msg = " | Length of list file text correct")
	checkEquals(length(run1@controlText), 36, msg = " |Length of control file text correct" )
	
	prob1 <- getProblem(run1)
	
	# test that the theta final estimates were as expected
	
	expThetas <- c(19.6, 84.6, 1.66)
	names(expThetas) <- paste("THETA", 1:3, sep = "")
	# rownames(expThetas) <- c("estimates") 
	checkEquals( getThetas(prob1, what = "final"),  expThetas, msg = " |theta final estimates correct")
	
	# test that omega final estimates imported as expected
	
	"%pst%" <- RNMImport:::"%pst%"
	expOmega <- array(diag(c(.164, .165, 1.3)), c(3,3), dimnames = list( "OMEGA" %pst% 1:3, "OMEGA" %pst% 1:3))
	checkEquals(expOmega, getOmegas(prob1, what = "final"), msg = " |omega final estimates imported as expected")
	
	# check that sigma final estimates imported as expected
	
	expSigma <- array(0.0202, c(1,1), dimnames = list("SIGMA1", "SIGMA1"))
	checkEquals(getSigmas(run1, what = "final"), expSigma, msg = " |sigma final estimates imported as expected")
	
	## check import of data
	
	allData <- nmData(run1, dataType = c("input", "output"), returnMode ="DFList")
	
	inData <- allData$input
	inDataTest <- as.matrix(rbind(head(inData, 2), tail(inData, 2)))
	rownames(inDataTest) <- NULL
	
	checkEquals(inDataTest, structure(c(1, 1, 65, 65, 2, 2, 1, 1, 55, 55, 64, 64, 2, 2, 1, 
							1, 154, 154, 180, 180, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 
							1, 1, 1, 10000, 0, 0, 0, 80.97, 80.97, 99.79, 99.79, 0, 1, 10, 
							12, 1, 1, 1.1, 1.1, 169.98, 114.57, 7.63, 6.15, 1, 0, 0, 0, 1, 
							0, 0, 0, 12, 0, 0, 0, 1, 1, 65, 65, 0, 0, 0, 0), .Dim = c(4L, 
							19L), .Dimnames = list(NULL, c("SID", "SEX", "AGE", "RACE", "HT", 
									"SMOK", "HCTZ", "PROP", "CON", "AMT", "WT", "TIME", "SECR", "DV", 
									"EVID", "SS", "II", "ID", "OCC"))), msg = " | input data correct")
	
	outData <- allData$output
	
	outDataTest <- as.matrix(rbind(head(outData, 2), tail(outData, 2)))
	rownames(outDataTest) <- NULL
	
	checkEquals(outDataTest, structure(c(1, 1, 65, 65, 0, 1, 10, 12, 8.2032, 113.58, 8.7492, 
		5.6597, 19.721, 0.0086953, -0.12792, 0.086636, 169.98, 114.57, 
	7.63, 6.15, 9.0449, 89.979, 7.1931, 4.5225, 0, 24.591, 0.43694, 
	1.6275, 0, 0.33079, -0.51811, 1.4561, 18.004, 18.004, 20.978, 
	20.978, 72.62, 72.62, 95.784, 95.784, 2.5569, 2.5569, 0.774, 
	0.774, 55, 55, 64, 64, 154, 154, 180, 180, 80.97, 80.97, 99.79, 
	99.79, 1, 1, 1.1, 1.1, 2, 2, 1, 1, 2, 2, 1, 1, 0, 0, 0, 0, 1, 
	1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 65, 65, 0, 
	0.33079, 0.51811, 1.4561), .Dim = c(4L, 24L), .Dimnames = list(
	    NULL, c("ID", "TIME", "IPRED", "IWRES", "DV", "PRED", "RES", 
	    "WRES", "CL", "V", "KA", "AGE", "HT", "WT", "SECR", "SEX", 
	    "RACE", "SMOK", "HCTZ", "PROP", "CON", "OCC", "SID", "absWRES"
	    ))), msg = " |output data correct")
	
	# check importNm works correctly with dropInputColumns = FALSE
	run2 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
				path = "(internalunit)", dropInputColumns = FALSE)
	
	run1.inData <- nmData(run1, dataType = "input")
	run2.inData <- nmData(run2, dataType = "input")
	
	checkTrue(setequal(names(run2.inData), c(names(run1.inData), "RATE")), msg = "Correct columns were dropped")	
	
	# now try importing a run without output tables
	
	run3 <- importNm("TestData1notab.ctl", path = file.path(unitTestPath, "testdata/TestRunNoTab"))
	prob3 <- getProblem(run3)
	
	checkException(nmData(prob3), msg = " |Output data missing, so there is an incompatibility in data size\n")
	
	# no output tables, so the output data set should be empty
	
	checkEquals(nmData(prob3, dataType = "output"), data.frame(), msg = " |output data is empty")
	
	checkEquals(list(getThetas(prob3), getOmegas(prob3), getSigmas(prob3)), 
			list(getThetas(prob1), getOmegas(prob1), getSigmas(prob1)), " |parameter estimates still the same, and available")
	
	obj <- getObjective(run1, addMinInfo = FALSE)
	
	checkEquals(obj, 3228.988)
	
	# check that importing with missing table files still works -

	run4 <- importNm("TestData1_missingtab.ctl","TestData1.lst" , path = file.path(unitTestPath, "testdata/TestRun"))
	checkEquals(nmData(run4, dataType = "output"), data.frame(), msg = " |empty output data" )
	
	# spot checks that other things were imported correctly
	checkEquals(getSigmas(run4), getSigmas(run3), msg = " |identical sigmas ")
	checkEquals(getThetas(run4), getThetas(run3), msg = " | identical thetas")
	
	checkEquals(nmData(run4, dataType = "input"), nmData(run3, dataType = "input"))
	
	#### check that a run with MAXEVAL=0 and no sigma declaration was imported correctly
	
	run5 <-  importNm("Test.ctl",path = file.path(unitTestPath, "testdata/TestNoSigMaxeval0" ))
	objective <- getObjective(run5)
	checkTrue(is.na(objective), msg = " |objective function not present!")
	checkEquals(attr(objective, "minInfo"), character(0), msg = " | missing objective minimization info")
	
	checkEquals( unname(getThetas(run5, what = "final")), unname(getThetas(run1, what = "initial")["initial",, drop = TRUE]), msg = " | final values of THETAS same as run 1 initial")
	checkEquals( unname(getOmegas(run5, what = "final")), unname(getSigmas(run1, what = "initial")), msg = " | final values of THETAS same as run 1 initial")
	
}

# tests for NMSimModel, still based on internal data (TestData/TestSimRun)

test.importNm.SimModel <- function()
{
	run2 <- importNm(conFile = "../TestSimRun/TestData1SIM.con", path = "(internalunit)")
	prob <- getProblem(run2)
	
	# check that the class of the problem is correct
	
	checkTrue(class(prob) == "NMSimModel", msg = " |problem class is correct")
	
	# check that the number of simulations is correct
	
	checkEquals(prob@numSimulations, 5, msg = " |5 simulations expected")
	
	# extract all parameter estimates 
	
	thetas <- getThetas(prob, subProblemNum = 1:5)
	omegas <- getOmegas(prob, subProblemNum = 1:5)
	sigmas <- getSigmas(prob, subProblemNum = 1:5)
	
	checkEquals(dim(thetas), c(5,3), msg = " |dimension of all extracted subproblems as expected")
	checkEquals(dim(omegas), c(3,3,5))
	checkEquals(dim(sigmas), c(1,1,5))
	
	checkTrue(all(thetas["sim3",] == c(18, 108, 1.24)), 
			msg = "Spot check for thetas, subproblem 3")
	checkTrue(all( thetas["sim1",] ==  c(17.2, 117, 1.24)),
			msg = "Spot check for thetas, subproblem 1")
	
	x <- omegas[,,"sim5"]
	y <- diag(c(0.174, 0.197, 1.47)) ;	dimnames(x) <- NULL
	checkEquals(x, y)
	x <- omegas[,,"sim4"] ; dimnames(x) <- NULL
	y <- diag(c(0.245, 0.189, 0.945))
	checkEquals(x, y)
	
	checkEquals(sigmas[,,"sim2"], 0.026)
	checkEquals(sigmas[,,"sim3"], 0.0262 )
	
	objectives <- getObjective(run2, subProblems = 1:5)
	y <- c(3696.247, 3575.252, 3660.355, 3606.526, 3701.472)
	names(y) <- paste("sim", 1:5, sep = "") 
	checkEquals(objectives, y)
	
	### check that importing with no tables still works
	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run4 <- importNm("TestData1SIM_missingtab.con","TestData1SIM.lst" , path = file.path(unitTestPath, "testdata/TestSimRun"))
	checkEquals(nmData(run4, dataType = "output"), structure(list(NSIM = structure(integer(0), .Label = c("1", "2", 
											"3", "4", "5"), class = c("ordered", "factor"))), .Names = "NSIM", row.names = integer(0), class = "data.frame"), msg = " |empty output data" )
	
	# spot checks that other things were imported correctly
	checkEquals(getSigmas(run4), getSigmas(run2), msg = " |identical sigmas ")
	checkEquals(getThetas(run4), getThetas(run2), msg = " | identical thetas")
	
	checkEquals(nmData(run4, dataType = "input"), nmData(run2, dataType = "input"))
	
	allData <- nmData(run2, dataType = c("input", "output"), returnMode ="DFList")
	
	inData <- allData$input
	inDataTest <- as.matrix(rbind(head(inData, 2), tail(inData, 2)))
	rownames(inDataTest) <- NULL
	
	checkEquals(inDataTest, structure(c(1, 1, 65, 65, 2, 2, 1, 1, 55, 55, 64, 64, 2, 2, 1, 
							1, 154, 154, 180, 180, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 
							1, 1, 1, 10000, 0, 0, 0, 80.97, 80.97, 99.79, 99.79, 0, 1, 10, 
							12, 1, 1, 1.1, 1.1, 169.98, 114.57, 7.63, 6.15, 1, 0, 0, 0, 1, 
							0, 0, 0, 12, 0, 0, 0, 1, 1, 65, 65, 0, 0, 0, 0), .Dim = c(4L, 
							19L), .Dimnames = list(NULL, c("SID", "SEX", "AGE", "RACE", "HT", 
									"SMOK", "HCTZ", "PROP", "CON", "AMT", "WT", "TIME", "SECR", "DV", 
									"EVID", "SS", "II", "ID", "OCC"))), msg = " | input data correct")
	
	outData <- subset(allData$output, sel = -NSIM)
	
	outDataTest <- as.matrix(rbind(head(outData, 2), tail(outData, 2)))
	rownames(outDataTest) <- NULL
	
	checkEquals(outDataTest, structure(c(1, 1, 65, 65, 0, 1, 10, 12, 18.908, 77.135, 28.666, 
							23.214, 7.9898, -0.0031396, 0.19734, 0.070985, 169.98, 76.893, 
							34.323, 24.862, 20.136, 73.154, 12.678, 9.2604, 0, 3.7388, 21.645, 
							15.601, 0, 0.063253, 3.6048, 1.9752, 16.414, 16.414, 9.1826, 
							9.1826, 99.378, 99.378, 87.059, 87.059, 1.1048, 1.1048, 3.7522, 
							3.7522, 55, 55, 64, 64, 154, 154, 180, 180, 80.97, 80.97, 99.79, 
							99.79, 1, 1, 1.1, 1.1, 2, 2, 1, 1, 2, 2, 1, 1, 0, 0, 0, 0, 1, 
							1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 65, 65, 0, 
							0.063253, 3.6048, 1.9752), .Dim = c(4L, 24L), .Dimnames = list(
							NULL, c("ID", "TIME", "IPRED", "IWRES", "DV", "PRED", "RES", 
									"WRES", "CL", "V", "KA", "AGE", "HT", "WT", "SECR", "SEX", 
									"RACE", "SMOK", "HCTZ", "PROP", "CON", "OCC", "SID", "absWRES"
							))), msg = " |output data correct" )
	
	# ensure that parsed report statements are captured properly
	# regression tests issue 2387 

	repStatements <- prob@reportStatements
	# should simply match what is given by importNmReport
	fullRep <- importNmReport( "../TestSimRun/TestData1SIM.lst", path = "(internalunit)" )
	checkEquals(repStatements, fullRep$problemResults[[1]], msg = " |report statments in problem object equal to those obtained via importNmReport") 
	
}

# import standard NONMEM 7 run

test.importNm.BasicNM7 <- function()
{
	# use pre-loaded run 
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	
	allThetas <- sapply(as.numeric(1:2), function(i) getThetas(run1, method = i, what = "final"))
	allOmegas <- lapply(as.numeric(1:2), function(i) getOmegas(run1, method = i, what = "final"))
	allSigmas <- lapply(as.numeric(1:2), function(i) getSigmas(run1, method = i, what = "final"))
	# FIXME: Theta standard errors not checked!
	thetaStderr <- getThetas(run1, method = 1, what = "stderrors")
	omegaStderr <- getOmegas(run1, method = 1, what = "stderrors")
	sigmaStderr <- getSigmas(run1, method = 1, what = "stderrors") 
		
	covMatrixMeth1 <- getEstimateCov( run1, method = 1, corMatrix = TRUE )
	covMatrixMeth2 <- getEstimateCov( run1, method = 2)
	
	checkTrue(is.null(covMatrixMeth2), msg = " |no covariance matrix for second method")
	
	# check thetas
	
	checkEquals(allThetas, structure(c(20, 77.3, 1.27, 19.1, 76.7, 1.68), .Dim = c(3L, 2L
					), .Dimnames = list(c("TH1", "TH2", "TH3"), NULL)), 
			msg = " |thetas imported correctly")
	
	# check sigmas
	
	checkEquals(allSigmas, list(structure(0.0259, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
									"EPS1"), methodName = "Iterative Two Stage"), structure(0.0266, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1"), methodName = "Stochastic Approximation Expectation-Maximization")),
			msg = " |sigmas imported correctly")
	
	
	# check omegas
	
	checkEquals( allOmegas, 
			list(structure(c(0.157, 0, 0, 0, 0.162, 0, 0, 0, 0.737), .Dim = c(3L, 
									3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
											"ETA3")),  methodName = "Iterative Two Stage"), 
					structure(c(0.145, 0, 0, 0, 0.149, 0, 0, 0, 1.42), .Dim = c(3L, 
									3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
											"ETA3")), methodName = "Stochastic Approximation Expectation-Maximization")),
			msg = " |omegas imported correctly")
	
	checkEquals(sigmaStderr
					, structure(0.000746, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
							"EPS1"), methodName = "Iterative Two Stage"))
	checkEquals(omegaStderr, structure(c(0.0426, 0, 0, 0, 0.0483, 0, 0, 0, 0.201), .Dim = c(3L, 
							3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
									"ETA3")), methodName = "Iterative Two Stage"))
	
	checkEquals( getObjective(run1, method = 1:2, addMinInfo = FALSE), c(3335.250, 2339.093) )
	
	# all shrinkages

	ETAShrinks <- lapply(as.numeric(1:2), function(i) getOmegas(run1, method = i, what = "shrinkage"))
	EPSShrinks <- lapply(as.numeric(1:2), function(i) getSigmas(run1, method = i, what = "shrinkage"))
	
	checkEqualsNumeric( ETAShrinks[[1]], c(5.0139, 12.1730, 14.9980 ), msg = " |ETA shrink correct for method 1")
	checkEqualsNumeric( ETAShrinks[[2]], c(0.072367, 6.459000, 5.190400), msg = " |ETA shrink correct for method 2")
	
	checkEqualsNumeric( EPSShrinks[[2]], -10.145, msg = " |EPS shrink correct for method 2" )
	checkEqualsNumeric( EPSShrinks[[1]], 12.013, msg = " |EPS shrink correct for method 1" )
	
	# iterations
	
	iter <- getIterations(run1)
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	expectedIter <- importNm7Tables("TestData1.EXT", path = file.path(unitTestPath, "testdata/TestDataNM7"), type = "ext")
	checkEquals(iter[[1]], expectedIter[[1]], check.attributes = FALSE, "msg = | iterations imported correctly")
	checkEquals(iter[[2]], expectedIter[[2]], check.attributes = FALSE, "msg = | iterations imported correctly")

	run2 <- importNm("TestData1_missingtab.ctl","TestData1.lst" , path = file.path(unitTestPath, "testdata/TestDataNM7"))
	
	checkEquals(nmData(run2, dataType = "output"), data.frame(), msg = " |empty output data" )
	
	# spot checks that other things were imported correctly
	checkEquals(getSigmas(run2), getSigmas(run1), msg = " |identical sigmas ")
	checkEquals(getThetas(run2), getThetas(run1), msg = " | identical thetas")
	
	checkEquals(nmData(run2, dataType = "input"), nmData(run1, dataType = "input"))
}