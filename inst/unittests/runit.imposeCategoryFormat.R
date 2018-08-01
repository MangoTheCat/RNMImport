test.imposeCategoryFormat <- function()
{
	
	#Test data frame 
	testDF <- data.frame(SEX = factor(c("male", "female", "female", "male", "female")), 
						 SMOK = factor(c("no", "yes", "yes", "no", "no")),
						 FOOD = factor(c("fasted", "fasted", "fed", "fed", "fed")))
	
	myDF <- data.frame(SEX = c(0, 1, 1, 0, 1), SMOK = c(0, 1, 1, 0, 0 ), FOOD = c(0, 0, 1, 1, 1))
	test1 <- imposeCategoryFormat(myDF, "SEX")
	test2 <- imposeCategoryFormat(myDF, c("SEX", "SMOK"))
	test3 <- imposeCategoryFormat(myDF, "SEX, SMOK")
	test4 <- imposeCategoryFormat(myDF)
	checkEquals(test1["SEX"], testDF["SEX"])
	checkEquals(test2[, c("SEX", "SMOK")], testDF[,c("SEX", "SMOK")])
	checkEquals(test3[, c("SEX", "SMOK")], testDF[,c("SEX", "SMOK")])
	checkEquals(test4, testDF)
	
	#Test NMBasic
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun"))
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	prob1 <- getProblem(run1)
	testB1 <- imposeCategoryFormat(prob1)
	checkEquals(levels(testB1@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB1@inputData[["SMOK"]]), c("no", "yes"))
	
	testB2 <- imposeCategoryFormat(prob1, c("SEX", "SMOK"))
	checkEquals(levels(testB2@outputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB2@outputData[["SMOK"]]), c("no", "yes"))
	
	testB3 <- imposeCategoryFormat(prob1, "SEX, SMOK")
	checkEquals(levels(testB3@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB3@inputData[["SMOK"]]), c("no", "yes"))
	
	testB4 <- imposeCategoryFormat(prob1, "SEX")
	checkEquals(levels(testB4@outputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB4@outputData[["SMOK"]]), NULL)
	checkEquals(levels(testB4@inputData[["SMOK"]]), NULL)
	
	
	#Test NMRun
	testR1 <- imposeCategoryFormat(run1)
	checkEquals(levels(testR1@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testR1@inputData[["SMOK"]]), c("no", "yes"))
	
	testR2 <- imposeCategoryFormat(prob1, c("SEX", "SMOK"))
	checkEquals(levels(testR2@outputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testR2@outputData[["SMOK"]]), c("no", "yes"))
	
	testR3 <- imposeCategoryFormat(prob1, "SEX, SMOK")
	checkEquals(levels(testR3@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testR3@inputData[["SMOK"]]), c("no", "yes"))
	
	testR4 <- imposeCategoryFormat(prob1, "SEX")
	checkEquals(levels(testR4@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testR4@inputData[["SMOK"]]), NULL)
	checkEquals(levels(testR4@outputData[["SMOK"]]), NULL)
	
	#Test NMSim
	run2 <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	prob2 <- getProblem(run2)
	testS1 <- imposeCategoryFormat(prob2)
	checkEquals(levels(testS1@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testS1@inputData[["SMOK"]]), c("no", "yes"))
	
	testS2 <- imposeCategoryFormat(prob2, c("SEX", "SMOK"))
	checkEquals(levels(testS2@outputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testS2@outputData[["SMOK"]]), c("no", "yes"))
	
	testS3 <- imposeCategoryFormat(prob2, "SEX, SMOK")
	checkEquals(levels(testS3@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testS3@inputData[["SMOK"]]), c("no", "yes"))
	
	testS4 <- imposeCategoryFormat(prob2, "SEX")
	checkEquals(levels(testS4@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testS4@inputData[["SMOK"]]), NULL)
	checkEquals(levels(testS4@outputData[["SMOK"]]), NULL)
	
	# NMBasicNM7
	run3 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	prob3 <- getProblem(run3)
	testB1 <- imposeCategoryFormat(prob3)
	checkEquals(levels(testB1@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB1@inputData[["SMOK"]]), c("no", "yes"))
	
	testB2 <- imposeCategoryFormat(prob3, c("SEX", "SMOK"))
	checkEquals(levels(testB2@outputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB2@outputData[["SMOK"]]), c("no", "yes"))
	
	testB3 <- imposeCategoryFormat(prob3, "SEX, SMOK")
	checkEquals(levels(testB3@inputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB3@inputData[["SMOK"]]), c("no", "yes"))
	
	testB4 <- imposeCategoryFormat(prob3, "SEX")
	checkEquals(levels(testB4@outputData[["SEX"]]), c("female", "male"))
	checkEquals(levels(testB4@outputData[["SMOK"]]), NULL)
	checkEquals(levels(testB4@inputData[["SMOK"]]), NULL)
}

