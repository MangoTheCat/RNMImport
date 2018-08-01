# $LastChangeDate: $
# $LastChangedBy: fgochez $
# $Rev: 13953 $
# 
# Author: fgochez
# Tests importNmMod
# Note that most of the functions that importNmMod makes use of have their own unit tests, so there is no need to 
# test this routine very thoroughly
###############################################################################


test.importNmMod <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testDir <- file.path(unitTestPath, "testdata")
	dat1 <- importNmMod(file.path(testDir, "control3.con"))
	
	dat1Class <- class(dat1)
	attributes(dat1Class) <- NULL
	
	checkEquals(dat1Class, "nmModel", msg = " |object returned by importNmMod now of its own class")
	checkEquals(length(dat1$Raw), 16, msg = "Length of control3\n" )
	# only one problem
	checkEquals(length(dat1$problemContents), 1)
	x <- dat1$problemContents[[1]]
	checkEquals(names(x), c("Theta", "Problem", "Tables", "Subroutine", "Input", "Data",
							"PK", "Error", "Estimates", "Cov") )
	checkEquals(x$Data[1,"REWIND"], c(REWIND = "FALSE"), msg = "Checking ")
	dat2 <- importNmMod(file.path(testDir, "control4.con"))
	checkEquals(rownames(dat2$problemContents[[1]]$Theta), c("A", "B", "C"), msg = "Parameter labels read correctly")

	dat3 <- importNmMod(file.path(testDir, "multiprob1.mod"))
	
	checkEquals(length(dat3$problemContents), 2)
	x <- dat3$problemContents
	checkEquals(names(x[[1]]), c("Problem","Subroutine", "Input", "Data", "PRED"   ))
	checkEquals(names(x[[2]]), c("Theta",  "Omega", "Sigma", "Problem", "Input","Data","Sim")) 
	# now try an example with simulation statements
	dat4 <- importNmMod(file.path(testDir, "subprob1.mod"))
	
	
	checkEquals(sum(dat4$Comments == ""), 59)
	checkEquals(dat4$Comments[6], " initialize" )
	checkEquals(dat4$Comments[43], " $SUPER SCOPE=3 ITERATIONS=10")
	
	x <- dat4$problemContents
	checkEquals(length(x), 4)
	
	checkEquals(x[[2]]$Theta[2,], c(.025,.102,.4), checkNames = FALSE )
	checkEquals(diag(x[[2]]$Omega), c(.04, .04, .04), checkNames = FALSE)
	# ensure that THETAs appear in the first 3 problems
	checkTrue(all(sapply(x[-4], function(y) "Theta" %in% names(y)) ))
	expectedSim <- c("1000", "5566898", "-1", "INITIAL", "TRUE")
	names(expectedSim) <- c("nSub", "Seed1", "Seed2", "TRUE", "simOnly" )
	#nSub     Seed1     Seed2      TRUE   simOnly 
	#"1000" "5566898"      "-1" "INITIAL"    "TRUE" 
	attr(expectedSim, "rawStatement") <- "(5566898) ONLY SUB=1000"
	checkEquals(x[[1]]$Sim, expectedSim )

}
