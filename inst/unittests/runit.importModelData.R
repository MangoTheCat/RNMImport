# SVN revision: $Rev: 115297 $
# Date of last change: $LastChangedDate: 2014-07-22 09:21:30 +0100 (Tue, 22 Jul 2014) $
# Last changed by: $LastChangedBy: jli@MANGO.LOCAL $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################
# test the importModelData function

test.importModelData <- function()
{
    .importNmModData <- RNMImport:::.importNmModData
    importModelData <- RNMImport:::importModelData
    
    # first test: checks that multiple IGNORE= are handled correctly
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
    testDir <- file.path(unitTestPath, "testdata")
    # x <- readNmData(file.path(testDir, "data3"))
    
    dataStatement <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)")
    
    inputStatement <- RNMImport:::.importNmModInput("$INPUT AMT TIME DV")
    
    testInput1 <- as.matrix(RNMImport:::importModelData(dataStatement, inputStatement, path = testDir))
    rownames(testInput1) <- NULL
    # checkEquals(testInput$TIME, 0, .27)
    checkEquals(testInput1, cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=c and IGNORE=list work correctly at the same time")
    # second test: 
    dataStatement2 <- RNMImport:::.importNmModData("$DATA data4.dat")
    inputStatement2 <- RNMImport:::.importNmModInput("$INPUT AMT TIME DV")
    
    testInput2 <- as.matrix(RNMImport:::importModelData(dataStatement2, inputStatement2, path = testDir))
    rownames(testInput2) <- NULL
    checkEquals( testInput2,  cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=# by default!" )
    
    # third tests: IGNORE=list used alone
    
    dataStatement3 <- RNMImport:::.importNmModData("$DATA data3 IGNORE=(TIME.LT.1)")
    inputStatement3 <- RNMImport:::.importNmModInput("$INPUT AMT TIME DV")
    
    testInput3 <- as.matrix( RNMImport:::importModelData(dataStatement3, inputStatement3, path = testDir) )
    rownames(testInput3) <- NULL
    
    checkEquals( testInput3,
            cbind(AMT = rep(NA, 8), 
                    TIME = c(1,1.92,3.5,5.02,7.03,9,12,24.3),
                    DV = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9)), 
            msg = " |IGNORE=code alone works as expected" )
    # test IGNORE = 'C'
    dataStatement4 <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE='I'")
    testInput4 <-  as.matrix( RNMImport:::importModelData(dataStatement4, inputStatement, path = testDir) )
    rownames(testInput4) <- NULL
    checkEquals( testInput4,  cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE='I' same as IGNORE=I " )
    
    dataStatement5 <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=\"I\"")
    testInput5 <-  as.matrix( RNMImport:::importModelData(dataStatement5, inputStatement, path = testDir) )
    rownames(testInput5) <- NULL
    checkEquals( testInput5,  cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=\"I\" same as IGNORE=I " )
    
    # check that LABEL=X is allowed
    
    dataStatement6 <- .importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME=1)")
    testInput6 <- as.matrix(importModelData(dataStatement6, inputStatement, path = testDir))
    rownames(testInput6) <- NULL
    
    checkEquals(testInput6, cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=(LABEL=X) works")
    
    dataStatement7 <- .importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1,DV.LT.2.01)")
    testInput7 <- as.matrix(importModelData(dataStatement7, inputStatement, path = testDir))
    rownames(testInput7) <- NULL
    
    checkEquals(testInput7, cbind(AMT = c(320, NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,0.52,1.92,3.5,5.02,9,12),
                    DV = c(NA, 7.91,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=(LABEL1.OP.X,LABEL2.OP.Y) works")
    
    # check that mixed IGNORE/IGN statements are allowed
    # related to issue 
    
    dataStatement8 <- .importNmModData("$DATA data3.dat IGN=I IGNORE=(TIME=1)")
    testInput8 <- as.matrix(importModelData(dataStatement8, inputStatement, path = testDir))
    rownames(testInput8) <- NULL
    checkEquals(testInput8, cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=(LABEL=X) works")
}

# test aliasing of columns
# function (dataStatement, inputStatement, dropCols = TRUE, trim = FALSE, 
# path = NULL, duplicateAliased = TRUE) 
test.importModelDataAliasDrop <- function()
{
    .importNmModData <- RNMImport:::.importNmModData
    importModelData <- RNMImport:::importModelData
    .importNmModInput <- RNMImport:::.importNmModInput
    
    # first test: checks that multiple IGNORE= are handled correctly
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
    testDir <- file.path(unitTestPath, "testdata")
    # x <- readNmData(file.path(testDir, "data3"))
    
    dataStatement1 <- .importNmModData("$DATA data4.dat IGNORE=#")
    inputStatement <- .importNmModInput("$INPUT AMT TIME=TIM DV=FOO")
    testInput1 <-  as.matrix(importModelData( dataStatement1, inputStatement, path = testDir ))
    
    
    rownames(testInput1) <- NULL
    
    checkEquals(testInput1, cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01),
                    TIM = c(0,.27,0.52,1,1.92,3.5,5.02,9,12), FOO = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01) ) ,
            " | aliasing works as expected")
    
    inputStatement2 <- .importNmModInput( "$INPUT AMT=DROP TIME=TIM DV=FOO")
    
    testInput2 <- as.matrix( importModelData( dataStatement1, inputStatement2, path = testDir, dropCols = TRUE ) )
    rownames(testInput2) <- NULL
    checkEquals(testInput2, cbind(  TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
                    DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01),
                    TIM = c(0,.27,0.52,1,1.92,3.5,5.02,9,12), FOO = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01) ), 
            msg = " |drop = TRUE works as expected")
    
    ## now check that IGNORE= can be applied with aliased columns
    
    
    dataStatement3 <- .importNmModData("$DATA data3 IGNORE=(TIME.LT.1)")
    dataStatement4 <- .importNmModData("$DATA data3 IGNORE=(TIM.LT.1)")
    
    testInput3 <- as.matrix( importModelData(dataStatement3, inputStatement, path = testDir) )
    rownames(testInput3) <- NULL
    
    checkEquals( testInput3,
            cbind(AMT = rep(NA, 8), 
                    TIME = c(1,1.92,3.5,5.02,7.03,9,12,24.3),
                    DV = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9), 
                    TIM = c(1,1.92,3.5,5.02,7.03,9,12,24.3), 
                    FOO = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9)), 
            msg = " | IGNORE = works with one name for TIME" )
    
    testInput4 <- as.matrix(  importModelData( dataStatement4, inputStatement, path = testDir ) )
    rownames(testInput4) <- NULL    
    checkEquals( testInput4, testInput3, msg = " |IGNORE = works for other alias of TIME"  )
    
    # another test: aliasing with extra columns
    
    inputStatement2 <- .importNmModInput("$INPUT MDV AMT EVID=DROP TIME CONC=DV")
    dataStatement5 <- .importNmModData("$DATA data5.csv IGNORE=(CONC=2,AMT.GT.10)")
    
    testInput5 <- as.matrix(  importModelData( dataStatement5, inputStatement2, path = testDir ) )
    
    checkEquals( testInput5, structure(c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 10L, 0L, 0L, 0L, 
                            0L, 0L, 0L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 9L, 8L, 7L, 
                            6L, 5L, 4L, 3L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 10L, 0L, 0L, 
                            0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 2L, 3L, 
                            4L, 5L, 6L, 7L, 8L, 0L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 0L, 9L, 8L, 
                            7L, 6L, 5L, 4L, 3L), .Dim = c(8L, 10L), .Dimnames = list(c("1", 
                                    "2", "3", "4", "5", "6", "7", "8"), c("MDV", "AMT", "TIME", "CONC", 
                                    "ExtraCol1", "ExtraCol2", "ExtraCol3", "ExtraCol4", "ExtraCol5", 
                                    "DV"))), msg = " |dropping columns with extra columns in data set works correctly" )
    
}

# tests model importing with ACCEPT=(list) statement

test.importModelData.accept <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
    testDir <- file.path(unitTestPath, "testdata")
    
    .importNmModData <- RNMImport:::.importNmModData
    importModelData <- RNMImport:::importModelData
    .importNmModInput <- RNMImport:::.importNmModInput
    
    dataStatement1 <- .importNmModData("$DATA data3 ACCEPT=(TIME.GE.1)")
    inputStatement <- .importNmModInput("$INPUT AMT TIME=TIM DV=FOO")
    
    testInput1 <- as.matrix( importModelData(dataStatement1, inputStatement, path = testDir) )
    rownames(testInput1) <- NULL
    
    checkEquals( testInput1,
            cbind(AMT = rep(NA, 8), 
                    TIME = c(1,1.92,3.5,5.02,7.03,9,12,24.3),
                    DV = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9), 
                    TIM = c(1,1.92,3.5,5.02,7.03,9,12,24.3), 
                    FOO = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9)), 
            msg = " | ACCEPT = works with one name for TIME" )
    
    # now check aliased version
    dataStatement2 <- .importNmModData("$DATA data3 ACCEPT=(TIM.GE.1)")
    
    testInput2 <- as.matrix( importModelData(dataStatement1, inputStatement, path = testDir) )
    rownames(testInput2) <- NULL
    
    checkEquals( testInput2,
            cbind(AMT = rep(NA, 8), 
                    TIME = c(1,1.92,3.5,5.02,7.03,9,12,24.3),
                    DV = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9), 
                    TIM = c(1,1.92,3.5,5.02,7.03,9,12,24.3), 
                    FOO = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9)), 
            msg = " | ACCEPT = works with another name for TIME" )
}


# tests model importing with RECORDS=n statement

test.importModelData.records <- function()
{
    .importNmModData <- RNMImport:::.importNmModData
    importModelData <- RNMImport:::importModelData
    .importNmModInput <- RNMImport:::.importNmModInput
    
    # first test: checks that RECORDS=n is handled correctly
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
    testDir <- file.path(unitTestPath, "testdata")
    
    
    dataStatement1 <- .importNmModData("$DATA data4.dat RECORDS=5")
    inputStatement <- .importNmModInput("$INPUT AMT TIME=TIM DV=FOO")
    testInput1 <-  as.matrix(importModelData( dataStatement1, inputStatement, path = testDir ))
    rownames(testInput1) <- NULL
    checkEquals(testInput1, cbind(AMT = c(320,NA,NA,NA,NA), 
                    TIME = c(0,.27,0.52,1,1.92), DV = c(NA,1.71,7.91,8.31,8.33),
                    TIM = c(0,.27,0.52,1,1.92), FOO = c(NA,1.71,7.91,8.31,8.33) ) ,
            " | Only first 5 records used")
    
    # second test: checks RECORD=label
    
    inputStatement2 <- .importNmModInput("$INPUT MDV AMT ID TIME CONC=DV")
    dataStatement2 <- .importNmModData("$DATA data5.csv RECORDS=ID")
    
    testInput2 <-  as.matrix(importModelData( dataStatement2, inputStatement2, path = testDir ))
    rownames(testInput2) <- NULL
    
    # test that only the first two rows were imported
    expectedData2 <- rbind( c(1,10,1,1,0), c(0,0,1,2,9) )
    colnames(expectedData2) <-  c("MDV", "AMT", "ID", "TIME", "CONC")
    checkEquals( testInput2[,1:5], expectedData2, msg = " |only first two rows imported" )
}