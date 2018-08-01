# SVN revision: $Rev: 29747 $
# Date of last change: $LastChangedDate: 2011-07-26 10:39:33 +0100 (Tue, 26 Jul 2011) $
# Last changed by: $LastChangedBy: fgochez $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests .importNmModData

test.importNmModData <- function()
{
	DATACOLNAMES <- c("File", "IG", "ACCEPT", "REWIND", "RECORDS", "TRANSLATE", "NULL")
	.importNmModData <- RNMImport:::.importNmModData
	
	dataStatements <- 
		c(
			"$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)", #1
			"$DATA data3.dat IGNORE=@", #2
			"$DATA data3.dat IGNORE='C'", #3 
			"$DATA data3.dat IGNORE=\"C\"", #4
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ.1)", #5
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME=1)", #6
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ.1,DV.LT.2.01)", #7
			"$DATA data3.dat REWIND IGNORE=I ACCEPT=(TIME.NE.1)", #8
			"$DATA data3.dat NOREWIND IGNORE=I ACCEPT=(TIME.NE.1)", #9
			"$DATA data3.dat IGN=@", #10
			"$DATA data3.dat IGN=I IGN=(TIME.EQ.1)", #11
			"$DATA data3.dat IGN=I IGNORE=(TIME.EQ.1)", #12
			"$DATA data3.dat IGN=I IGNORE(TIME.EQ.1)", #13
			"$DATA data3.dat IGNORE=I IGN(TIME.EQ.1)", #14
			"$DATA data3.dat", #15
			"$DATA data3.dat IGNORE=I IGN(TIME .EQ. 1)", # 16 issue 4961 (same for next 2)
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ. 1,DV .LT.2.01)", #17
			"$DATA data3.dat IGN(DV .LT.2.01)" #18
			
		)
	
	dataExpected <-
		list(
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",   #1
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="@", ACCEPT="",     #2
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="C", ACCEPT="",     #3
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="C", ACCEPT="",     #4
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="", #5     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME=1)", ACCEPT="",    #6
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1,DV.LT.2.01)", ACCEPT="", # 7     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I", ACCEPT="(TIME.NE.1)",  #8 
				REWIND="TRUE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I", ACCEPT="(TIME.NE.1)",  #9 
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="@", ACCEPT="",     #10
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",   #11  
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="(TIME.EQ.1);I", ACCEPT="",   #12
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="(TIME.EQ.1);I", ACCEPT="", #13   
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="", #14
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),			
			c( File="data3.dat", IG="NONE", ACCEPT="", #15     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",   #16
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1,DV.LT.2.01)", ACCEPT="", #17     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="(DV.LT.2.01)", ACCEPT="",     #18
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= "" )	
		)
		
	allDataTests <- t(sapply(dataStatements, .importNmModData))
	rownames(allDataTests) <- NULL
	allDataExpected <- do.call(rbind, dataExpected)
	colnames(allDataExpected) <- colnames(allDataTests)
	
	checkEquals(allDataTests, allDataExpected, msg = " all inputs and outputs as expected")
	
}