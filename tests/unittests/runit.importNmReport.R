# FIXME: make these tests more comprehensive!!
# Note: a great deal of the functionality of importNmReport is covered by other unit tests, such
# as importNm, so these tests will not be extensive


test.importNmReport.Basic <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	report1 <- importNmReport("TestData1notab.lst", path = file.path(unitTestPath, "testdata/TestRunNoTab"))
	
	report1Class <- class(report1)
	attributes(report1Class) <- NULL
	checkEquals( report1Class, "nmRunReport", msg = " |class of returned object is correct")
	
	checkEquals(report1$VersionInfo, structure(c("VI", "2.0"), .Names = c("Version", "Level")), msg = " | correct version info")
	
	 #conStatements <- importNmMod("TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun"))
	# report1.withCtl <- importNmReport("TestData1.lst", controlStatements = conStatements, path = file.path(unitTestPath, "testdata/TestRun"))
	# checkEquals(report1, report1.withCtl, "report loaded with control statements and without are identical (1)")
	
	checkEquals(length(report1$Raw), 231, msg = "Raw contents correct length")
	
	probRes <- report1$problemResults[[1]]
	checkTrue(setequal(names(probRes), c("nRecords", "nIndividuals", "Objective.Minimum", "FinalEstimates","Iter",
							"InverseCovarianceMatrix", "CovarianceMatrix", "CorrelationMatrix", "StandardError")), 
			"checking presence of correct elements in report")
	
	checkEquals( probRes$FinalEstimates, structure(list(THETA = structure(c(19.6, 84.6, 1.66), .Names = c("TH1", 
											"TH2", "TH3")), OMEGA = structure(c(0.164, 0, 0, 0, 0.165, 0, 
											0, 0, 1.3), .Dim = c(3L, 3L), .Dimnames = list(c("ETA1", "ETA2", 
													"ETA3"), c("ETA1", "ETA2", "ETA3"))), SIGMA = structure(0.0202, .Dim = c(1L, 
											1L), .Dimnames = list("EPS1", "EPS1"))), .Names = c("THETA", 
							"OMEGA", "SIGMA")), msg = " | final estimates correct" )
	
	checkEquals( probRes$StandardError, structure(list(THETA = structure(c(0.963, 3.56, 0.218), .Names = c("TH1", 
											"TH2", "TH3")), OMEGA = structure(c(0.0239, 0, 0, 0, 0.022, 0, 
											0, 0, 0.345), .Dim = c(3L, 3L), .Dimnames = list(c("ETA1", "ETA2", 
													"ETA3"), c("ETA1", "ETA2", "ETA3"))), SIGMA = structure(0.00322, .Dim = c(1L, 
											1L), .Dimnames = list("EPS1", "EPS1"))), .Names = c("THETA", 
							"OMEGA", "SIGMA")), msg = " |standard error correct" )
	checkEquals(c(probRes$nIndividuals, probRes$nRecord), c(99, 887))
	checkEquals(probRes$Objective.Minimum, 3228.988 )
	##################
	# test_stoptimefooter.lst
	# checks that it is possible to import a file with the footer of the type
	#Stop Time: 
	# Tue 01/26/2010 
	# 11:17 AM
	#################
	
	report2 <- importNmReport("test.lst", path = file.path(unitTestPath, "testdata") )
	report3 <- importNmReport("test_stoptimefooter.lst", path = file.path(unitTestPath, "testdata") )
	
	# compare the reports without the raw contents.  They should be equal
	
	checkEquals( tail(report2, -1), tail(report3, - 1 ) )
	# check that the sigma matrix has not been misread, as it is the last element and could cause problems
	checkEquals( report3$problemResults[[2]]$FinalEstimates$SIGMA, structure(0.0197, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
							"EPS1")) )
}

test.importNmReport.SimModel <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	report2 <- importNmReport("TestData1SIM.lst", path = file.path(unitTestPath, "testdata/TestSimRun"))
	conStatements <- importNmMod("TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun"))
	report2.withCtl <- importNmReport("TestData1SIM.lst", controlStatements = conStatements, path = file.path(unitTestPath, "testdata/TestSimRun"))
	
	checkEquals(report2$VersionInfo , c("VI", "1.0"), check.attributes = FALSE , msg = " | correct version info")
	
	checkEquals(report2, report2.withCtl, "report loaded with control statements and without are identical (2)")
	probRes <- report2$problemResults[[1]]
	checkEquals(names(probRes), c("nRecords", "nIndividuals", "FinalEstimates"), "checking presence of correct elements in report" )
	checkEquals(c(probRes$nIndividuals, probRes$nRecord), c(99, 887))
	checkEquals(probRes$FinalEstimates, 
			structure(list(THETA = structure(c(17.2, 18.3, 18, 19, 17.1, 
											117, 108, 108, 98.9, 109, 1.24, 1.4, 1.24, 1.55, 1.38), .Dim = c(5L, 
											3L), .Dimnames = list(c("sim1", "sim2", "sim3", "sim4", "sim5"
											), c("TH1", "TH2", "TH3"))), OMEGA = structure(c(0.174, 0, 0, 
											0, 0.19, 0, 0, 0, 1.45, 0.167, 0, 0, 0, 0.143, 0, 0, 0, 1.24, 
											0.181, 0, 0, 0, 0.142, 0, 0, 0, 1.57, 0.245, 0, 0, 0, 0.189, 
											0, 0, 0, 0.945, 0.174, 0, 0, 0, 0.197, 0, 0, 0, 1.47), .Dim = c(3L, 
											3L, 5L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", 
													"ETA2", "ETA3"), c("sim1", "sim2", "sim3", "sim4", "sim5"))), 
							SIGMA = structure(c(0.0241, 0.026, 0.0262, 0.0295, 0.0244
									), .Dim = c(1L, 1L, 5L), .Dimnames = list("EPS1", "EPS1", 
											c("sim1", "sim2", "sim3", "sim4", "sim5"))), Objective.Minimum = structure(c(3696.247, 
											3575.252, 3660.355, 3606.526, 3701.472), .Names = c("sim1", 
											"sim2", "sim3", "sim4", "sim5"))), .Names = c("THETA", "OMEGA", 
							"SIGMA", "Objective.Minimum")), msg = " | ")
	
}

test.importNmReport.BasicNM7 <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	report3 <- importNmReport("TestData1.lst", path = file.path(unitTestPath, "testdata/TestDataNM7"))
	
	probResults <- report3$problemResults[[1]]
	
	checkEquals( report3$VersionInfo, c("Version" = "VII", "Level" = "1.0"), 
			msg = " |correct version")
	methResults <- probResults$MethodResults
	
	allThetas <- sapply(methResults, function(x) x$FinalEstimates$THETA)
	allSigmas <- lapply(methResults, function(x) x$FinalEstimates$SIGMA)
	allOmegas <- lapply(methResults, function(x) x$FinalEstimates$OMEGA)
	
	# check thetas
	
	checkEquals(allThetas, structure(c(20, 77.3, 1.27, 19.1, 76.7, 1.68), .Dim = c(3L, 2L
					), .Dimnames = list(c("TH1", "TH2", "TH3"), NULL)), 
			msg = " |thetas imported correctly")
	
	# check sigmas
	
	checkEquals(allSigmas, list(structure(0.0259, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
									"EPS1")), structure(0.0266, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1"))),
		msg = " |sigmas imported correctly")

	
	# check omegas
	
	checkEquals( allOmegas, 
			list(structure(c(0.157, 0, 0, 0, 0.162, 0, 0, 0, 0.737), .Dim = c(3L, 
									3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
											"ETA3"))), structure(c(0.145, 0, 0, 0, 0.149, 0, 0, 0, 1.42), .Dim = c(3L, 
									3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
											"ETA3")))),
			msg = " |omegas improted correctly")

	allThetaStderr <- sapply(methResults, function(x) x$StandardError$THETA)
	allSigmaStderr <- lapply(methResults, function(x) x$StandardError$SIGMA)
	allOmegaStderr <- lapply(methResults, function(x) x$StandardError$OMEGA)
	
	checkTrue(is.null(allThetaStderr[[2]]) & is.null(allSigmaStderr[[2]])
			 & is.null(allOmegaStderr[[2]]))
	checkEquals(allSigmaStderr[[1]], structure(0.000746, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
							"EPS1")))
	checkEquals(allOmegaStderr[[1]], structure(c(0.0426, 0, 0, 0, 0.0483, 0, 0, 0, 0.201), .Dim = c(3L, 
							3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
									"ETA3"))))
	# check dimensions of correlation and covariance matrices
	checkEquals(dim(methResults[[1]]$CorrelationMatrix), c(10, 10))
	checkEquals(dim(methResults[[1]]$CovarianceMatrix), c(10, 10))
	checkEquals(dim(methResults[[1]]$InverseCovarianceMatrix), c(10, 10))
	
	# check shrink values

	checkEquals( methResults[[1]]$ETAshrink, c(5.0139, 12.1730, 14.9980 ), msg = " |ETA shrink correct for method 1")
	checkEquals( methResults[[2]]$ETAshrink, c(0.072367, 6.459000, 5.190400), msg = " |ETA shrink correct for method 2")
	
	checkEquals( methResults[[2]]$EPSshrink, -10.145, msg = " |EPS shrink correct for method 2" )
	checkEquals( methResults[[1]]$EPSshrink, 12.013, msg = " |EPS shrink correct for method 1" )
	
	##################
	# test_stoptimefooter.lst
	# checks that it is possible to import a file with the footer of the type
	#Stop Time: 
	# Tue 01/26/2010 
	# 11:17 AM
	#################
	
	report4 <- importNmReport("TestData1_stoptimefooter.lst", path = file.path(unitTestPath, "testdata"))
	
	checkEquals( tail( report3, - 1), tail(report4, -1  ) )
}
