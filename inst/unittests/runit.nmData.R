
# Test nmData for an object of class "NMBasicModel" and "NMRun"
#

test.nmData.NMBasic <- function()
{

	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	prob <- getProblem(run1)
	test1 <- nmData(run1)
	checkEquals(test1, nmData(getProblem(run1)), "Extracting from run and problem directly should give same result")
	test2 <- nmData(run1, dataType = "input")
	inputColumns <- c("SID", "SEX" ,"AGE" ,"RACE",  "HT",  "SMOK",  "HCTZ","PROP", "CON", "AMT", "WT" ,"TIME", "SECR", "DV", "EVID", "SS", "II" ,"ID", "OCC")
	outputColumns <- c("ID", "TIME", "IPRED", "IWRES", "CL", "V", "KA", "AGE", "HT", "WT", "SECR", "SEX", "RACE", 
				"SMOK", "HCTZ", "PROP", "CON", "OCC", "absWRES", "DV", "WRES", "PRED", "RES", "SID") 
	
	checkTrue(setequal(names(test2), inputColumns), "Checking that all input data is present")

	test3 <- nmData(run1, dataType = "output")
	checkTrue(setequal(names(test3), outputColumns), "Checking that all output data is present")
	test4 <- nmData(prob, returnMode = "DFList")
	checkEquals(names(test4), c("input", "output"), "Check return list names")
	checkEquals(test2, test4$input)
	checkEquals(test3, test4$output)
	
	x <- union( inputColumns, outputColumns	)
	y <- intersect(inputColumns, outputColumns )
	checkTrue(setequal(names(test1), c(x, paste(y, ".INPUT", sep = ""))))
	# check error handling
	
	checkEquals( test2, nmData(prob, dataType = c("input", "blah")), msg = " |invalid type 'blah'discarded")
	try.nmData <- try( nmData( prob, dataType = "out" ) )
	
	checkTrue(inherits(try.nmData, "try-error"), msg = " | Exception generated when trying to extract invalid data type")
	checkTrue(length( grep( try.nmData, pattern =  "No valid datatypes selected for retrieval" )) > 0, msg = " |correct exception generated")
	
	# tests added for subsetting logic:
	
	test5 <- nmData( prob, subset = TRUE )
	checkEquals(test5, nmData(prob, subset = dataSubset(prob)), 
			msg = " subset = TRUE and subset = dataSubset(prob) are identical")
	
	checkEquals(nmData(prob, subset = FALSE), nmData(prob), msg = " subset = FALSE and susbet = NULL are equivalent")
	
	TESTSUBSET <- "ID == 1"
	
	test6.input <- nmData(prob, dataType = "input", subset = "ID == 1")
	test6.output <- nmData(prob, dataType = "output", subset = "ID == 1")
	
	test6.inoutlist <- nmData(prob, returnMode = "DFList", subset = "ID == 1")
	
	checkEquals(test6.input, test6.inoutlist$input, msg = " subset from input extracted correctly into a list" )
	checkEquals(test6.output, test6.inoutlist$output, msg = " subset from output only extracted correctly into a list" )
	
	test6.inoutdf  <- nmData(prob, returnType = "singleDF", subset = "ID == 1")
	
	checkEquals( test6.inoutdf, subset(nmData(prob), ID == 1 ))
	# check that subset is passed to NMRun method correctly
	
	checkEquals(nmData(prob, subset = TRUE), nmData(run1, subset = TRUE), " subset is passed to NMRun method correctly")
	
	# check fix for case 2298
	# modify test data so that it only has one column repeated in output and input
# SEX.INPUT AGE.INPUT RACE.INPUT HT.INPUT SMOK.INPUT HCTZ.INPUT PROP.INPUT CON.INPUT WT.INPUT TIME.INPUT SECR.INPUT DV.INPUT ID.INPUT OCC.INPUT
	
	prob@outputData <- subset(prob@outputData, select = - c(AGE, RACE, HT, SMOK, HCTZ, PROP, CON, WT, TIME, SECR, DV, ID, OCC, SID) )
	test2298 <- nmData(	prob )
	
	checkTrue( setequal( colnames(test2298), c("IPRED", "IWRES", "PRED", "RES", "WRES", "CL", "V", "KA", "SEX", 
							"absWRES", "SID", "AGE", "RACE", "HT", "SMOK", "HCTZ", "PROP", 
							"CON", "AMT", "WT", "TIME", "SECR", "DV", "EVID", "SS", "II", 
							"ID", "OCC", "SEX.INPUT")
	), msg = "Single clashing column is now correct" )
}

# test nmData for an object of class "NMSimDataGen" and "NMSimModel"

test.nmData.NMSim <- function()
{
	# run1 <- importNm("TestData1SIM.con", "TestData1SIM.lst", path = file.path(unitTestPath, "testdata/TestSimRun"))
	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run2 <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	prob <- getProblem(run2)

	dataSubset(prob) <- NULL
	inputColumns <- c("SID", "SEX" ,"AGE" ,"RACE",  "HT",  "SMOK",  "HCTZ","PROP", "CON", "AMT", "WT" ,"TIME", "SECR", "DV", "EVID", "SS", "II" ,"ID", "OCC")
	outputColumns <- c("ID", "TIME", "IPRED", "IWRES", "CL", "V", "KA", "AGE", "HT", "WT", "SECR", "SEX", "RACE", 
			"SMOK", "HCTZ", "PROP", "CON", "OCC", "absWRES", "DV", "WRES", "PRED", "RES", "SID") 
	
	test1 <- nmData(prob, dataType = "input")
	checkEquals(nmData(prob, subProblemNum = 2 ), nmData(getProblem(run2), subProblemNum = 2), 
			"Extracting from run and problem directly should give same result")
	test2 <- nmData(prob, dataType = "output")
	checkTrue(setequal(names(test1), inputColumns), msg = "Checking presence of input data")
	checkTrue(setequal(names(test2), c(outputColumns, "NSIM")), msg = "Checking presence of output data")
	 
	test3 <- nmData(prob, subProblemNum = 2:3, returnMode = "DFList") 
	checkEquals(nrow(test3[["input"]]) * 2, nrow(test3[["output"]]), msg = "Number of rows of output is correct" )
	
	checkEqualsNumeric(as.numeric(test3[["output"]]$NSIM), c(rep(2, 1061 ), rep(3, 1061)), msg= "NSIM set correctly" )
	
	test4 <- nmData(prob, subProblemNum = 5)
	x <- union( inputColumns, outputColumns	)
	y <- intersect(inputColumns, outputColumns )
	
	checkTrue(setequal(names(test4), c(x, "NSIM", paste(y, ".INPUT", sep = ""))), msg = "correct columns present")
	checkTrue(any(test4$DV != test4$"DV.INPUT"), msg = "simulated DV not equiavlent to input DV")
	
	# extract multiple simulations into a single DF
	
	test5 <- nmData(prob, subProblemNum = 1:2)
	checkEquals(nrow(test5), 2 * 1061, msg = " result has the correct number of rows")
	checkEquals(test5$NSIM, factor(c(rep(1, 1061), rep(2, 1061)), levels = as.character(1:5), ordered = TRUE))
	
	
	test6 <- nmData( prob, subset = TRUE )
	checkEquals(test6, nmData(prob, subset = dataSubset(prob)), 
			msg = " subset = TRUE and subset = dataSubset(prob) are identical")
	
	checkEquals(nmData(prob, subset = FALSE), nmData(prob), msg = " subset = FALSE and susbet = NULL are equivalent")
	
	# check error handling

	try.nmData <- try( nmData( prob, dataType = "out" ) )
	
	checkTrue(inherits(try.nmData, "try-error"), msg = " | Exception generated when trying to extract invalid data type")
	checkTrue(length( grep( try.nmData, pattern =  "No valid datatypes selected for retrieval" )) > 0, msg = " |correct exception generated")

	
	# test subsetting
	
	TESTSUBSET <- "ID == 1"
#	
	test7.input <- nmData(prob, dataType = "input", subset = "ID == 1")
	test7.output <- nmData(prob, dataType = "output", subset = "ID == 1")
#	
	test7.inoutlist <- nmData(prob, returnMode = "DFList", subset = "ID == 1")
#	
	checkEquals(test7.input, test7.inoutlist$input, msg = " subset from input extracted correctly into a list" )
	checkEquals(test7.output, test7.inoutlist$output, msg = " subset from output only extracted correctly into a list" )
#	
	test7.inoutdf  <- nmData(prob, returnMode = "singleDF", subset = "ID == 1")
#	
	checkEquals( test7.inoutdf, subset(nmData(prob), ID == 1 ))
	
	prob@outputData <- subset(prob@outputData, select = - c(AGE, RACE, HT, SMOK, HCTZ, PROP, CON, WT, TIME, SECR, DV, ID, OCC, SID) )
	test2298 <- nmData(	prob )
	
	checkTrue( setequal( colnames(test2298), c("IPRED", "IWRES", "PRED", "RES", "WRES", "CL", "V", "KA", "SEX", 
							"absWRES", "SID", "AGE", "RACE", "HT", "SMOK", "HCTZ", "PROP", 
							"CON", "AMT", "WT", "TIME", "SECR", "DV", "EVID", "SS", "II", 
							"ID", "OCC", "SEX.INPUT", "NSIM")
			), msg = "Single clashing column is now correct" )
}

test.nmDatabyVarType <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	run1 <- importNm("TestData1.ctl", "TestData1.lst", path = file.path(unitTestPath, "testdata/TestRun"))
	test1 <- nmDatabyVarType(run1, varTypes = "Covariate")
	checkEquals(dim(test1), c(1061, 4), "dimensions of the data are correct")
	checkTrue(setequal(names(test1), c("AGE", "RACE", "SEX", "SMOK")))
	test2 <- nmDatabyVarType(run1, varTypes = "Parameter")
	checkEquals(nrow(test2), 1061)
	checkTrue(setequal(names(test2), c("CL", "KA", "V")), msg = "correct variables selected")
	test3 <- nmDatabyVarType(run1, varTypes = "Eta")
	test4 <- nmDatabyVarType(run1, varTypes = "Lab covariate")
	checkEquals(c(ncol(test3), ncol(test4) ), c(0, 0), "no etas or lab covariates")
	test5 <- nmDatabyVarType(run1, varTypes = c("Covariate", "Parameter"))
	checkEquals(test5, cbind(test1, test2))
	test6 <- nmDatabyVarType(run1, varTypes = c("Covariate", "Parameter"), returnMode = "DFList")
	checkEquals(test6[[1]], test1)
	checkEquals(test6[[2]], test2)
	
	run2 <- importNm("TestData1SIM.con", "TestData1SIM.lst", path = file.path(unitTestPath, "testdata/TestSimRun"))
	
	test7 <- nmDatabyVarType(run2, varTypes = "Covariate", subProblemNum  = 2)
	test7.2 <- test7
	rownames(test7.2) <- as.character(seq_len(nrow(test7)))
	checkEquals(test7.2, test1, "simulated and non-simulated data equivalent for this cast")
}