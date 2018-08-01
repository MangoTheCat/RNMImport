
test.createCategorical.edgeCase <- function()
{
	# check pathological breaks when using quantiles on highly "skewed" data.  
	
	strangeDf <- data.frame(X = c(1.78, -1.02,  0.74,  1.24, -0.74,  0.59, -0.32,  0.69, -0.14,1.40, 
					rep(0, 100)) )
	newDf <- addDerivedCategorical(strangeDf, "X", newVar = "X.CUT", binType = "counts" )
	checkEquals(levels(newDf$"X.CUT"), c("[-1.02,0]", "(0,1.78]"), msg = "Only 2 unique levels in quantile")
	checkTrue(all(newDf[["X.CUT"]][11:100] == "[-1.02,0]"), msg = "All elements past first 10 in the correct bin")
}

# tests creation of categorical variables
#
# 

test.createCategorical <- function()
{
	testDF <- data.frame(A = c(10, 20, 50, 100, 150, 200, 250, 300, 400, 500), B = letters[1:10],
						 A.CUT = factor(c(10, 20, 50, 100, 150, 200, 250, 300, 400, 500)),
						 CutByRange = factor(c("(9.51,108]", "(9.51,108]", "(9.51,108]", "(9.51,108]", "(108,206]", "(108,206]", "(206,304]",
											"(206,304]", "(304,402]", "(402,500]"), 
												levels = c("(9.51,108]", "(108,206]", "(206,304]", "(304,402]", "(402,500]")),
						 ExplicitCuts = factor(c("[0,50]", "[0,50]", "[0,50]", "(50,100]", "(100,150]", "(150,300]",
										 	"(150,300]", "(150,300]", "(300,500]", "(300,500]"), 
												levels = c("[0,50]", "(50,100]", "(100,150]", "(150,300]", "(300,500]")),
						 ExplicitCutsLabels = factor(c("BIN1", "BIN1", "BIN1", "BIN2", "BIN3", "BIN4", "BIN4", "BIN4", "BIN5", "BIN5")),
						 LogCuts = factor(c("(2.3,3.28]", "(2.3,3.28]", "(3.28,4.26]", "(4.26,5.24]", "(4.26,5.24]", "(5.24,6.22]", "(5.24,6.22]",
								 			"(5.24,6.22]", "(5.24,6.22]", "(5.24,6.22]"),
												levels =c("(2.3,3.28]", "(3.28,4.26]", "(4.26,5.24]", "(5.24,6.22]")),
						 EqualCountCuts = factor(c("[10,175]", "[10,175]", "[10,175]", "[10,175]", "[10,175]", "(175,500]", 
										 	"(175,500]", "(175,500]", "(175,500]", "(175,500]"), 
												levels = c("[10,175]", "(175,500]")), 
													stringsAsFactors = FALSE)
	
	#Force name of third column factors	
	names(testDF)[names(testDF) == "A.CUT"] <- "A.CUT"

	#Test Exceptions
	checkException(addDerivedCategorical(testDF, varName = "D"))
	checkException(addDerivedCategorical(testDF, varName = "B"))
	checkException(addDerivedCategorical(testDF, varName = "A", binType = "newmethod"))
	
	#Test Different Cuts
	myDF <- data.frame(A = c(10, 20, 50, 100, 150, 200, 250, 300, 400, 500), B = letters[1:10], stringsAsFactors = FALSE)
	testOne <- addDerivedCategorical(myDF, varName = "A", binType = "unique")
	testTwo <- addDerivedCategorical(testOne, varName = "A", breaks = 5, newVar = "CutByRange")
	testThree <- addDerivedCategorical(testTwo, varName = "A", breaks = c(0, 50, 100, 150, 300, 500), newVar = "ExplicitCuts", binType = "explicitcuts")
	testFour <- addDerivedCategorical(testThree, varName = "A", breaks = c(0, 50, 100, 150, 300, 500), labels = c("BIN1", "BIN2", "BIN3", "BIN4", "BIN5"), 
					newVar = "ExplicitCutsLabels", 	binType = "explicitcuts")
	testFive <-addDerivedCategorical(testFour, varName = "A", newVar = "LogCuts", binType = "logrange", breaks = 4)
	testSix <- addDerivedCategorical(testFive, varName = "A", newVar = "EqualCountCuts", binType = "counts", breaks = 2)
	checkEquals(names(testSix), names(testDF))
	for(i in 1:ncol(testDF)){
		checkEquals(attr(testSix[,i], "levels"), attr(testDF[,i], "levels"))
		checkEquals(attr(testSix[,i], "class"), attr(testDF[,i], "class"))
		checkEquals(as.character(testSix[,i]), as.character(testDF[,i]))
	}
	
	#Perform some simple tests on NMRun, NMBasicModel and NMSimModel
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun"))
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	prob1 <- getProblem(run1)
	
	#Check NMBasic
	testBas <- addDerivedCategorical(prob1, varName = "WT", breaks = 6, dataType = "input")
	checkException(addDerivedCategorical(prob1, varName = "WT", breaks = 6, dataType = "Intermediate"))
	checkEquals(nrow(testBas@additionalVars), nrow(prob1@inputData))
	checkEquals(names(testBas@additionalVars), "WT.CUT")
	checkEquals(length(levels(testBas@additionalVars[["WT.CUT"]])), 6) 
		
	#Check NMRun
	testRun <- addDerivedCategorical(run1, varName = "SEX", binType = "unique", newVar = "FactorSEX")
	checkException(addDerivedCategorical(run1, varName = "SEX",  binType = "unique", dataType = "Intermediate"))
	checkEquals(nrow(testRun@additionalVars), nrow(testRun@outputData))
	checkEquals(names(testRun@additionalVars), "FactorSEX")
	checkEquals(length(levels(testRun@additionalVars[["FactorSEX"]])), 2)
	
}

# very simple test 

test.createCategorical.NMBasicNM7 <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testRun <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	testProb <- getProblem(testRun)
	testProb2 <- addDerivedCategorical(testProb, "DV")
	
	testFrame <- nmData(testProb)
	expFrame <- addDerivedCategorical(testFrame, "DV")
	checkEquals(addedData(testProb2)$DV.CUT, expFrame$DV.CUT, msg = " |add derived categorical works for NMBasicModelNM7"  )
}

test.createCategorical.NMSim <- function()
{
	#Check NMSim
	run2 <- importNm(conFile = "../TestSimRun/TestData1SIM.con", path = "(internalunit)")
	prob2 <- getProblem(run2)
	testSim1 <- addDerivedCategorical(prob2, varName = "DV", breaks = 5, binType = "counts", newVar = "CountsDV")
	checkException(addDerivedCategorical(prob2, varName = "DV", breaks = 5, binType = "counts", newVar = "CountsDV", dataType = "Intermediate"))
	checkEquals(nrow(testSim1@additionalVars), nrow(testSim1@outputData))
	checkEquals(names(testSim1@additionalVars), "CountsDV")
	checkEquals(length(levels(testSim1@additionalVars[["CountsDV"]])), 5)
	
	testSim2 <- addDerivedCategorical(prob2, varName = "WT", breaks = c(0, 25, 50, 100, 150), binType = "explicitcuts", newVar = "ExplicitWTCut", dataType = "input")
	checkException(addDerivedCategorical(prob2, varName = "WT", breaks = c(0, 25, 50, 100, 200), binType = "explicitcuts", dataType = "Intermediate"))
	checkEquals(nrow(testSim2@additionalVars), nrow(testSim2@outputData))
	checkEquals(names(testSim2@additionalVars), "ExplicitWTCut")
	checkEquals(length(levels(testSim2@additionalVars[["ExplicitWTCut"]])), 4)
}

# tests the addedData function.  Moved here for cleaner code

test.addedData <- function()
{

	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	prob1 <- getProblem(run1)
	
	# check NMBasicModel
	
	testBas <- addDerivedCategorical(prob1, varName = "WT", breaks = 6, dataType = "input")	
	addedDataTest <- addedData(testBas)
	checkEquals(class(addedDataTest), "data.frame", " |Added data extracted is a data.frame")
	checkEquals( names( addedDataTest ) ,"WT.CUT", " |Correct column names" )
	checkEquals(dim(addedDataTest), c(1061, 1), " |Dimensions of extracted data are corrected")
	checkEquals(levels(addedDataTest$"WT.CUT"), c("(50.9,65.7]", "(65.7,80.5]", "(80.5,95.2]", "(95.2,110]", "(110,125]", "(125,139]")  )
	
	# test for run method
	
	testRun2 <- addDerivedCategorical(run1, varName = "WT", breaks = 6, dataType = "input")
	addedDataTest2 <- addedData(testRun2)
	checkEquals(class(addedDataTest2), "data.frame", " |Added data extracted is a data.frame")
	checkEquals( names( addedDataTest2 ) ,"WT.CUT", " |Correct column names" )
	checkEquals(dim(addedDataTest2), c(1061, 1), " |Dimensions of extracted data are corrected")
	checkEquals(levels(addedDataTest2$"WT.CUT"), c("(50.9,65.7]", "(65.7,80.5]", "(80.5,95.2]", "(95.2,110]", "(110,125]", "(125,139]")  )
	
}
	