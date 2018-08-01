# TODO: Add comments
# 
# Author: rweeks
###############################################################################


test.getVarDescription <- function()
{
	varDescription <- RNMImport:::getVarDescription
	testFrameOne <- data.frame(Variable = c("ID", "SEX"), Label = c("Subject Number", "Gender"),
						Format = c("", "0=male, 1=female"),	VarType = c("Undefined", "Covariate"), stringsAsFactors = FALSE)
	testFrameTwo <- data.frame(Variable = "CLCR", Label = "Creatinine Clearance (mL/min)", Format = "", VarType = "Lab Covariate", stringsAsFactors = FALSE)
	
	x <- varDescription(c("ID", "SEX"))
	rownames(x) <- NULL
	
	checkEquals(x, testFrameOne, check.attributes = FALSE)	
	checkEquals(names(varDescription(c("ID", "SEX"))), names(testFrameOne))
	checkEquals(varDescription("CLCR"), testFrameTwo, check.attributes = FALSE)
	checkEquals(names(varDescription(c("CLCR"))), names(testFrameTwo))
	# TODO: add test for when there are missing variable descriptions
}

test.setVarDescription <- function()
{
	df <- RNMImport:::.RNMImportEnv$variables
	getVar <- RNMImport:::getVarDescription
	setVar <- RNMImport:::setVarDescription
	checkException(setVar("NONSENSE"))
	testFrame <- data.frame(Variable = "SEX", Label = "M/F", Format = "0=male,1=female",
						VarType = "Unknown", stringsAsFactors = FALSE)
	setVar(varName = "SEX", varLabel = "M/F", varFormat = "0=male,1=female", varType = "Unknown")
	checkEquals(getVar("SEX"), testFrame, check.attributes = FALSE)
	checkEquals(names(getVar("SEX")), names(testFrame))
	assign("variables", df, envir = RNMImport:::.RNMImportEnv)
}

test.addVarDescription <- function()
{
	df <- RNMImport:::.RNMImportEnv$variables
	addVar <- RNMImport:::addVarDescription
	getVar <- RNMImport:::getVarDescription
	checkException(addVar(varName = "ID", varLabel = "Sequence ID", varType = "Unknown"))
	testFrame <- data.frame(Variable = "XXX", Label = "RUNIT Test", Format = "", VarType = "Undefined", stringsAsFactors = FALSE)
	addVar(varName = "XXX", varLabel = "RUNIT Test", varType = "Undefined")
	checkEquals(getVar("XXX"), testFrame, check.attributes = FALSE)
	checkEquals(names(getVar("SEX")), names(testFrame))
	assign("variables", df, envir = RNMImport:::.RNMImportEnv)
}

test.getNmFileExtensions <- function()
{
	fileExtension <- RNMImport:::getNmFileExtensions
	checkException(fileExtension("NONSENSE"))
	checkEquals(fileExtension("control"), c("mod", "ctl", "con"))
	checkEquals(fileExtension("report"), c("lst", "out", "sep"))
	checkEquals(fileExtension("table"), c("fit", "tab"))
	checkEquals(fileExtension("input"), c("", "txt", "dat"))
}

test.setNmFileExtensions <- function()
{
	original <- RNMImport:::.RNMImportEnv$fileExtensions
	setFileExts <- RNMImport:::setNmFileExtensions
	getFileExts <- RNMImport:::getNmFileExtensions
	checkException(setFileExts("NONSENSE", c("blah", "blah")))
	setFileExts("control", "testc1")
	setFileExts("report", c("testl1", "testl2"))
	setFileExts("table", c("testt1", "testt2", "testt3"))
	setFileExts("input", c("testi1", "testi2", "testi3", "testi4"))
	checkEquals(getFileExts("control"), "testc1")
	checkEquals(getFileExts("report"), c("testl1", "testl2"))
	checkEquals(getFileExts("table"), c("testt1", "testt2", "testt3"))
	checkEquals(getFileExts("input"), c("testi1", "testi2", "testi3", "testi4"))
	assign("fileExtensions", original, envir = RNMImport:::.RNMImportEnv)
}

test.getNmPath <- function()
{
	tmp <- original <-RNMImport:::.RNMImportEnv$dataPath
	getPath <- RNMImport:::getNmPath
	tmp[["Dummy"]] <- "X:\\Program Files\\Dummy Data"	
	assign("dataPath", tmp, envir = RNMImport:::.RNMImportEnv)	
	checkEquals(getPath("Dummy"), "X:\\Program Files\\Dummy Data") 
	assign("dataPath", original, envir = RNMImport:::.RNMImportEnv)
}

test.setNmPath <- function()
{
	original <-RNMImport:::.RNMImportEnv$dataPath
	getPath <- RNMImport:::getNmPath
	setPath <- RNMImport:::setNmPath
	setPath("Dummy", "X:\\Program Files\\Dummy Data")	
	checkEquals(getPath("Dummy"), "X:\\Program Files\\Dummy Data") 
	assign("dataPath", original, envir = RNMImport:::.RNMImportEnv)
}

test.removeNmPath <- function()
{
	original <-RNMImport:::.RNMImportEnv$dataPath
	setPath <- RNMImport:::setNmPath
	getPath <-  RNMImport:::getNmPath
	removePath <- RNMImport:::removeNmPath
	assign("dataPath", character(0), envir = RNMImport:::.RNMImportEnv)
	testVectorOne <- c("X:\\Program Files\\Dummy Data 1", "X:\\Program Files\\Dummy Data 2")
	names(testVectorOne) <- c("Dummy1", "Dummy2")
	testVectorTwo <- "X:\\Program Files\\Dummy Data 1"
	names(testVectorTwo) <- "Dummy1"
	setPath("Dummy1", "X:\\Program Files\\Dummy Data 1")	
	setPath("Dummy2", "X:\\Program Files\\Dummy Data 2")
	setPath("Dummy3", "X:\\Program Files\\Dummy Data 3")
	removePath("Dummy3")
	checkEquals(get("dataPath", envir = RNMImport:::.RNMImportEnv), testVectorOne)
	removePath("Dummy2")
	checkEquals(get("dataPath", envir = RNMImport:::.RNMImportEnv), testVectorTwo)
	removePath("Dummy1")
	checkEquals(get("dataPath", envir = RNMImport:::.RNMImportEnv), character(0))
	assign("dataPath", envir = RNMImport:::.RNMImportEnv, original)
}

# Tests whether or not the subset configuration functions work correctly 

test.configSubsets <- function()
{
	STANDARDSUBSET <- c("MDV != 1", "EVID == 0", "AMT <= 0")
	# by default, should attach subsets
	checkTrue( RNMImport:::applySubsetOnLoad(), msg = " |By default, data subset should be applied|" )
	
	# check default is loaded correctly.
	
	checkEquals(defaultDataSubset(), STANDARDSUBSET,
			msg = " |Default data subset as expected|")
	
	
	# check that the subset can be modified
	
	setDefaultDataSubset("MDV != 1", TRUE)
	checkEquals(defaultDataSubset(),"MDV != 1", " |Default data subset changed as expected|")
	
	# check that it can be augmented
	augmentDefaultDataSubset("EVID == 0")
	checkEquals(defaultDataSubset(),c("MDV != 1", "EVID == 0"), " |Default data subset augmented as expected|")
	
	# check that duplicates eliminated when augmenting
	
	augmentDefaultDataSubset("EVID == 0")
	checkEquals(defaultDataSubset(),c("MDV != 1", "EVID == 0"), " |Default data subset augmented as expected, no duplicates|")
		
	# check that when loaded, subset is attached to object
	# set internal unit path from runRNMImportTests?
	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testRun <- importNm(conFile = "TestData1notab.ctl", reportFile = "TestData1notab.lst", 
			path =  file.path(unitTestPath, "testdata/TestRunNoTab"))
	
	checkEquals(dataSubset(getProblem(testRun)), c("MDV != 1", "EVID == 0"), " |Subset loaded correctly|")
	
	# disable subsetting
	setDefaultDataSubset(STANDARDSUBSET, FALSE)
	
	# TODO: remove use of the global unitTestPath!!
	
	
	testRun <- importNm(conFile = "TestData1notab.ctl", reportFile = "TestData1notab.lst", 
			path = file.path(unitTestPath, "testdata/TestRunNoTab"))
	
	checkTrue(is.null(dataSubset(testRun)),  " |No default data subset attached|")
	
	# reset configuration
	RNMImport:::applySubsetOnLoad(TRUE)
	checkTrue(RNMImport:::applySubsetOnLoad())
}

# Tests whether or not the subset configuration functions handle errors as expected.

test.configSubsetsErrorHandling <- function()
{
	checkException(setDefaultDataSubset(1:10, TRUE), " |Non-character subset not allowed|")
	checkException(setDefaultDataSubset("MDV != 1", 1), " |Non-logical toggle not allowed|" )
	checkException(augmentDefaultDataSubset(1:10)," |Non-character subset not allowed|" )
}