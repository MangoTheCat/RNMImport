# $LastChangedBy: fgochez $
# $LastChangedDate: 2010-01-13 14:43:01 +0000 (Wed, 13 Jan 2010) $
# $Rev: 14659 $
# TODO: Add comment
# 
# Author: rweeks
###############################################################################

.removeMethName <- RNMImport:::.removeMethName

# tests getThetas to ensure that  behaviour under various combinations of parameters works as expected
# note that this function is also tested in runit.importNm.R, so this focuses more on
# parameter combinations than anything else


test.getThetas <- function()
{
	#Check NMBasicModel and NMRun
	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	prob1 <- getProblem(run1)
	
	run3 <- importNm( "TestData1notab.ctl", path = file.path(unitTestPath, "testdata/TestRunNoTab" ))
	# expThetas <- matrix(c(19.6, 84.6, 1.66), 1, 3, dimnames = list(c("estimates"), c("THETA1", "THETA2", "THETA3")))
	expThetas <- c("THETA1" = 19.6, "THETA2" = 84.6, "THETA3" = 1.66)
	expThetaInitial <- matrix(c(-Inf, 18.7, Inf, -Inf, 87.3, Inf, -Inf, 2.13, Inf), 3,
						dimnames = list(c("lowerBound", "initial", "upperBound"), c("THETA1", "THETA2", "THETA3")))
	expThetaStderrs <-  c("THETA1" = 0.963, "THETA2" = 3.56, "THETA3" = 0.218)
	checkEquals(getThetas(prob1), expThetas)
	checkEquals(getThetas(run1), expThetas)
	
	# final and initial together
	
	checkEquals(getThetas(prob1, what = c("final", "initial")), rbind( expThetaInitial, "estimates" = expThetas),
				msg = " |what = final and initial works correctly")
	
	tryStderrs <- try(getThetas(run1, what = "stderrors"), silent = TRUE)
	checkTrue(length(grep(tryStderrs, pattern = "Standard errors not available")) > 0, 
			msg = " | correct error message when no standard errors available")
	# stderrors and final
	checkEquals(getThetas(run3, what = c("final", "stderrors")), 
			rbind("estimates" = expThetas, "standardErrors" = expThetaStderrs), 
			msg = " | final and stderrs together is correct" )
	
	############ Check NMSimModel
	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testDir2 <- file.path(unitTestPath, "testdata/TestSimRun")
	
	run2 <- importNm("TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun"))
	
	prob2 <- getProblem(run2)
	
	simThetas <- matrix(c(17.2, 18.3, 18.0, 19.0, 17.1, 117.0, 108.0, 108.0, 98.9, 109.0, 1.24, 1.40, 1.24, 1.55, 1.38), 5, 3)
	rownames(simThetas) <- c("sim1", "sim2", "sim3", "sim4", "sim5")
	simInitial <- c(19.6, 84.6, 1.66)
	
	#names(simInitial) <- c("THETA1", "THETA2", "THETA3")
	
	checkEquals(getThetas(prob2, subProblemNum = 1:5),  simThetas, msg = " |for sim model, final as expected")
	
	checkEquals(getThetas(prob2, subProblemNum = 3:4, what = c("final", "initial")),  
			list("initial.estimates" = simInitial, "final.estimates" = simThetas[3:4,] ), 
			msg = " |for sim model, final + initial as expected")
	
	checkEquals(getThetas(prob2, what = "initial"), simInitial, msg = " |initial alone as expected")
	
	## check that parameters are passed through correctly from run to simModel
	# see also issue 1969
	
	checkEquals(getThetas(run2, subProblemNum = 1:3, what = c("initial", "final")), 
			getThetas(prob2, subProblemNum = 1:3,  what = c("initial", "final") ))
	
	######## NONMEM 7 run
	
	run3 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	prob3 <- getProblem(run3)
	
	checkEquals( getThetas(prob3, what = c("initial", "final"), method = 2), 
			structure(c(-Inf, 20, Inf, 19.1, -Inf, 77.3, Inf, 76.7, -Inf, 
							1.27, Inf, 1.68), .Dim = c(4L, 3L), .Dimnames = list(c("lowerBound", 
									"initial", "upperBound", "estimates"), c("THETA1", "THETA2", 
									"THETA3")), methodName = "Stochastic Approximation Expectation-Maximization"))
	
	checkEquals(getThetas(prob3, what = "initial", method = 1), structure(c(-Inf, 18.7, Inf, -Inf, 87.3, Inf, -Inf, 2.13, Inf
					), .Dim = c(3L, 3L), .Dimnames = list(c("lowerBound", "initial", 
									"upperBound"), c("THETA1", "THETA2", "THETA3")), 
					methodName = "Iterative Two Stage"))
	
	checkEquals(getThetas(prob3, what = "final"),
	            structure(c(19.1, 76.7, 1.68), 
	                      .Names = c("TH1", "TH2", "TH3"), 
	                      methodName = "Stochastic Approximation Expectation-Maximization"))

	thetaTest4 <- getThetas(prob3, what = "final", method = 1:2)
	thetaTest5 <- getThetas(prob3, what = c("initial", "final", "stderrors"), method = 1:2 )
	
	checkEquals(thetaTest4, list(getThetas(prob3, what = "final", method = 1), 
					getThetas(prob3, what = "final", method = 2)), msg = " |multiple methods correct (1) " )
	
	checkEquals(thetaTest5, list(getThetas(prob3, what = c("initial", "stderrors", "final"), method = 1), 
					getThetas(prob3, what = c("initial", "stderrors", "final"), method = 2)), 
			msg = " |multiple methods correct (2)")
	
	testDir <- file.path(unitTestPath, "testdata/TestRun")	
	mod1 <- importNmMod("TestData1.ctl",  path = testDir)
	checkEquals(getThetas(mod1), t(mod1$problemContents[[1]]$Theta))
}

test.getOmegas <- function()
{
	########### Check NMBasicModel and NMRun
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testDir1 <- file.path(unitTestPath, "testdata/TestRun")
	
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	prob1 <- getProblem(run1)
	
	expOmegas <- array(c(0.164, 0, 0, 0, 0.165, 0, 0, 0, 1.3), c(3,3))
#	dimnames(expOmegas) = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3"), "estimates)
	
	dimnames(expOmegas) = list(paste("OMEGA", 1:3, sep = ""), paste("OMEGA", 1:3, sep = ""))
	expOmegaInit <- diag(c(0.128, 0.142, 1.82))
	dimnames(expOmegaInit) = list(c("OMEGA1", "OMEGA2", "OMEGA3"), c("OMEGA1", "OMEGA2", "OMEGA3"))
	
	checkEquals(getOmegas(prob1),  expOmegas, msg = " |default parameters : extract final values")
	checkEquals(getOmegas(run1),  expOmegas)
	
	omegaTest1 <- getOmegas(prob1, what = "initial")
	omegaTest2 <- getOmegas(run1, what = "initial")
	
	checkEquals(omegaTest1,  expOmegaInit)
	checkEquals(omegaTest2,  expOmegaInit)	
	
	run3 <- importNm( "TestData1notab.ctl", path = file.path(unitTestPath, "testdata/TestRunNoTab" ))
	omegaTest3 <- getOmegas(run3, what = c("final", "stderrors"))
	omegaStderrs <- structure(c(0.0239, 0, 0, 0, 0.022, 0, 0, 0, 0.345), .Dim = c(3L, 
					3L), .Dimnames = list(c("OMEGA1", "OMEGA2", "OMEGA3"), c("OMEGA1", 
							"OMEGA2", "OMEGA3")))
	checkEquals(omegaTest3, list(final.estimates = expOmegas, 
					standard.errors = omegaStderrs), msg = " | final + standard errors as expected")
	
	# check exception handling

	tryOmega <- try(getOmegas(run3, what = "foo"))
	checkTrue( inherits(tryOmega, "try-error") )
	checkTrue(length(grep( tryOmega, pattern = "No valid items selected for retrieval!" )) > 0)
	
	# try to get standard errors that aren't there, check for correct error message
	
	tryOmega2 <- try(getOmegas(run1, what = "stderrors"))
	checkTrue( inherits(tryOmega2, "try-error") )
	checkTrue(length(grep( tryOmega2, pattern = "Standard errors not available" )) > 0)
	
	#Check NMSimModel
	
	
	# run2 <- importNm(conFile = "testdata1sim.con", reportFile = "testdata1sim.lst", 
	#		path = testDir2)
	
	run2 <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	prob2 <- getProblem(run2)
	
	simOmegas <- array(dim = c(3,3,5), dimnames = list(paste("OMEGA", 1:3, sep = ""), 
					c(paste("OMEGA", 1:3, sep = "")), 
					c("sim1", "sim2", "sim3", "sim4", "sim5")))
	simOmegas[, , 1] <- diag(c(0.174, 0.19, 1.45)); simOmegas[, , 2] <- diag(c(0.167, 0.143, 1.24))
	simOmegas[, , 3] <- diag(c(0.181, 0.142, 1.57)); simOmegas[, , 4] <- diag(c(0.245, 0.189, 0.945))
	simOmegas[, , 5] <- diag(c(0.174, 0.197, 1.47))
	initialSimOmegas <- diag(c(0.164, 0.165, 1.3)) 
	
	dimnames(initialSimOmegas) <- list(c("OMEGA1", "OMEGA2", "OMEGA3"), c("OMEGA1", "OMEGA2", "OMEGA3"))
	
	checkEquals(getOmegas(prob2, subProblemNum = 1:5),  simOmegas)
	checkEquals(getOmegas(prob2, what = "initial", subProblemNum = 1:5),  initialSimOmegas)
	
	checkEquals(getOmegas(prob2, what = c("final", "initial"), subProblemNum = 1:5), 
			list("initial.estimates" = initialSimOmegas , "final.estimates" = simOmegas))
	
	checkEquals(getOmegas(prob2, what = c("final", "initial"), subProblemNum = 4), 
			list("initial.estimates" = initialSimOmegas , "final.estimates" = simOmegas[,,4, drop = FALSE]))
	# check that parameters passed down correctly (see also issue 1969)
	checkEquals(getOmegas(run2, subProblemNum = 1:3, what = c("initial", "final")), 
			getOmegas(prob2, subProblemNum = 1:3,  what = c("initial", "final") ))
	
	
	###################################
	# NMBasicModelNM7
	#
	##################################
	
	run3 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	prob3 <- getProblem(run3)
	
	omegaTest1 <- getOmegas(prob3, what = c("initial", "final"), method = 2)
	
	expOmega1 <- list(initial.estimates = .removeMethName( getOmegas(prob3, what = "final", method = 1) ),	
			final.estimates = .removeMethName(getOmegas(prob3, method = 2)))
	
	attr(expOmega1, "methodName") <- "Stochastic Approximation Expectation-Maximization"
	
	checkEquals(omegaTest1, expOmega1,	msg = " | getOmegas correct (1)")
	
	omegaTest2 <- getOmegas(prob3, what = c("stderrors", "initial"), method = 1)
	
	checkEquals(omegaTest2, 
			structure(list(initial.estimates = structure(c(0.128, 0, 0, 0, 
											0.142, 0, 0, 0, 1.82), .Dim = c(3L,  3L), .Dimnames = list(c("OMEGA1", 
													"OMEGA2", "OMEGA3"), c("OMEGA1", "OMEGA2", "OMEGA3"))), 
											standard.errors = structure(c(0.0426, 
											0, 0, 0, 0.0483, 0, 0, 0, 0.201), .Dim = c(3L, 3L), .Dimnames = list(
											c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3")))), .Names = c("initial.estimates", 
							"standard.errors"), methodName = "Iterative Two Stage"), 
			msg = " | getOmegas correct (2)")
	
	omegaTest3 <- getOmegas( prob3, what = c("final"))
	
	checkEquals(omegaTest3, 
	            structure(c(0.145, 0, 0, 0, 0.149, 0, 0, 0, 1.42), 
	                      .Dim = c(3L,3L), 
	                      .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3")), 
	                      methodName = "Stochastic Approximation Expectation-Maximization"),
			 	msg = " | getOmegas correct (3)")
		
	## add additional tests for extracting multple methods at once
	
	omegaTest4 <- getOmegas(prob3, what = "final", method = 1:2)
	omegaTest5 <- suppressWarnings(getOmegas(prob3, what = c("final", "stderrors"), method = 1:2 ))
	
	checkEquals(omegaTest4, list(getOmegas(prob3, what = "final", method = 1), 
					getOmegas(prob3, what = "final", method = 2)), msg = " |multiple methods correct (1) " )
	
	checkEquals(omegaTest5, list(getOmegas(prob3, what = c("stderrors", "final"), method = 1), 
					suppressWarnings(getOmegas(prob3, what = c("stderrors", "final"), method = 2))), 
			msg = " |multiple methods correct (2)")
	
	testDir <- file.path(unitTestPath, "testdata/TestRun")	
	mod1 <- importNmMod("TestData1.ctl",  path = testDir)
	checkEquals(getOmegas(mod1), mod1$problemContents[[1]]$Omega)
}

test.getSigmas <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1notab.ctl", path = file.path(unitTestPath, "testdata/TestRunNoTab" ))
	prob1 <- getProblem(run1)
	
	expSigmas <- array(0.0202, c(1,1), dimnames = list("SIGMA1", "SIGMA1"))
	expSigmaStderrs <- array(0.00322, c(1,1), dimnames = list("SIGMA1", "SIGMA1"))
	expSigmaInit <- matrix(0.0231, 1,dimnames = list("SIGMA1", "SIGMA1")) 
	
	
	checkEquals(getSigmas(prob1),  expSigmas)
	checkEquals(getSigmas(run1),  expSigmas)
	checkEquals(getSigmas(prob1, what = "initial"),  expSigmaInit)
	checkEquals(getSigmas(run1, what = "initial"),  expSigmaInit)
	
	checkEquals(getSigmas(prob1, what = c("initial", "stderrors")), list("initial.estimates" = expSigmaInit, 
					"standard.errors" = expSigmaStderrs) )
	
	run3 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	# try to get standard errors that aren't there, check for correct error message
	
	trySigmas2 <- try(getSigmas(run3, what = "stderrors"))
	checkTrue( inherits(trySigmas2, "try-error") )
	checkTrue(length(grep( trySigmas2, pattern = "Standard errors not available" )) > 0)
	
	#Check NMSimModel
	run2 <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	
	prob2 <- getProblem(run2)
	
	simSigmas <- array(c(0.0241, 0.026, 0.0262, 0.0295, 0.0244), dim = c(1,1,5), 
			dimnames = list("SIGMA1", "SIGMA1", c("sim1", "sim2", "sim3", "sim4", "sim5")))
	sigmaInitial <- matrix(0.0202, 1, dimnames = list("SIGMA1", "SIGMA1"))
	
	checkEquals(getSigmas(prob2, subProblemNum = 2:4),  simSigmas[,, 2:4, drop = FALSE ])
	checkEquals(getSigmas(prob2, what = "initial", subProblemNum = 1:5), sigmaInitial)
	checkEquals(getSigmas(prob2, what = c("final", "initial"), subProblemNum = 3), 
			list("initial.estimates" = sigmaInitial, "final.estimates" = simSigmas[,,3, drop = FALSE] ) )
	
	# check that parameters passed down correctly (see also issue 1969)
	
	checkEquals(getSigmas(run2, subProblemNum = 1:3, what = c("initial", "final")), 
			getSigmas(prob2, subProblemNum = 1:3,  what = c("initial", "final") ))
	
	
	###############################
	#
	# NMBasicModelNM7
	#
	################################

	run3 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	prob3 <- getProblem(run3)
	
	sigmaTest1 <- getSigmas(prob3, what = c("initial", "final"), method = 2)
	
	
	expSigma1 <- list(initial.estimates = .removeMethName( getSigmas(prob3, what = "final", method = 1) ),	
			final.estimates = .removeMethName(getSigmas(prob3, method = 2)))
	
	attr(expSigma1, "methodName") <- "Stochastic Approximation Expectation-Maximization"
	
	checkEquals(sigmaTest1, expSigma1,
			msg = " | getSigmas correct (1)")
	
	sigmaTest2 <- getSigmas(prob3, what = c("stderrors", "initial"), method = 1)
	
	checkEquals(sigmaTest2, 
			structure(list(initial.estimates = structure(0.0231, .Dim = c(1L, 
											1L), .Dimnames = list("EPS1", "EPS1")), standard.errors = structure(0.000746, .Dim = c(1L, 
											1L), .Dimnames = list("EPS1", "EPS1"))), .Names = c("initial.estimates", 
							"standard.errors"), methodName = "Iterative Two Stage"), 
			msg = " | getSigmas correct (2)")
	
	sigmaTest3 <- getSigmas( prob3, what = "final" )
	
	checkEquals(sigmaTest3,	
	            structure(0.0266, .Dim = c(1L, 1L), 
	                      .Dimnames = list("EPS1","EPS1"), 
	                      methodName = "Stochastic Approximation Expectation-Maximization"))
	
	# check multiple methods at once
	
	sigmaTest4 <- getSigmas(prob3, what = "final", method = 1:2)
	sigmaTest5 <- suppressWarnings(getSigmas(prob3, what = c("initial", "final", "stderrors"), method = 1:2))
	
	checkEquals(sigmaTest4, list(getSigmas(prob3, what = "final", method = 1), 
					getSigmas(prob3, what = "final", method = 2)), msg = " |multiple methods correct (1) " )
	
	checkEquals(sigmaTest5, list(getSigmas(prob3, what = c("initial", "stderrors", "final"), method = 1), 
					suppressWarnings(getSigmas(prob3, what = c("initial", "stderrors", "final"), method = 2))), 
			msg = " |multiple methods correct (2)")
	
	# check nmModel
	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testDir <- file.path(unitTestPath, "testdata/TestRun")	
	mod1 <- importNmMod("TestData1.ctl",  path = testDir)
	checkEquals(getSigmas(mod1), mod1$problemContents[[1]]$Sigma)
	
}
