#
# $Rev: 13387 $
# $LastChangedDate: 2009-11-24 09:11:04 +0000 (Tue, 24 Nov 2009) $
# $LastChangedBy: fgochez $
###############################################################################

# tests the .importNmModPrior function

test.importNmModPrior <- function()
{
	
	# simple test to see if 2 $PRIOR statements are correctly parsed
	
	priorText <- "$PRIOR NWPRI NTHETA=5, NETA=4, NTHP=0, NETP=4, NPEXP=1"
	priorTest1 <- RNMImport:::.importNmModPrior(priorText)
	
	expected1 <- c(nTheta = 5, nEta = 4, nThp = 0,  npExp = 1, nEtp = 4)
	attr(expected1, "NWPRI") <- TRUE
	checkEquals(priorTest1, expected1 , "| Prior statement correctly parsed" )
	
	priorText2 <- "$PRIOR NWPRI NTHETA=2, NETA=5, NTHP=0, NETP=5, NPEXP=1"
	priorTest2 <- RNMImport:::.importNmModPrior(priorText2)
	
	expected2 <- c(nTheta = 2, nEta = 5, nThp = 0,  npExp = 1, nEtp = 5)
	attr(expected2, "NWPRI") <- TRUE
	
	checkEquals(priorTest2, expected2 , "| Prior statement (2) correctly parsed" )
}
