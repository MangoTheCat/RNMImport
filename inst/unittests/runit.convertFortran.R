# $LastChangedDate: 2010-01-19 10:48:38 +0000 (Tue, 19 Jan 2010) $
# $LastChangedBy: fgochez $
# $Rev: 14790 $
# 
# Author: fgochez
###############################################################################


# test the convertFortran95Ops function

test.convertFortran <- function()
{
	codeBlock1 <- "ID/=1.AND.TIME>0"
	
	checkEquals(RNMImport:::convertFortran95Ops(codeBlock1), "ID.NE.1.AND.TIME.GT.0")
	
	codeBlock2 <- "AMT==250.OR.AMT==260"
	
	checkEquals(RNMImport:::convertFortran95Ops(codeBlock2), "AMT.EQ.250.OR.AMT.EQ.260")
	
	codeBlock3 <- "TIME<=100"
	checkEquals(RNMImport:::convertFortran95Ops(codeBlock3), "TIME.LE.100")
	
	codeBlock4 <- "X>200.AND.Y>=1000"
	checkEquals(RNMImport:::convertFortran95Ops(codeBlock4), "X.GT.200.AND.Y.GE.1000")
	
	codeBlock5 <- "X<200.AND.Y<=1000"
	checkEquals(RNMImport:::convertFortran95Ops(codeBlock5) , "X.LT.200.AND.Y.LE.1000")
	
}
