# $Rev: 13658 $
# $LastChangedDate: 2009-11-30 15:54:28 +0000 (Mon, 30 Nov 2009) $
# $LastChangedBy: fgochez $
# Author: fgochez
###############################################################################

# tests .importNmModEst function

test.importNmModEst <- function()
{
	estText <- c(
	"$EST METHOD=ITS INTERACTION FILE=test7.EXT NOTITLE=1  NITER=100 PRINT=5 NOABORT SIGL=8 CTYPE=3 CITER=10",
	"NOPRIOR=1 CALPHA=0.05 NSIG=2",
	"$EST METHOD=SAEM INTERACTION NBURN=200 NITER=300 SIGL=8 ISAMPLE=2 PRINT=10 SEED=1556678 CTYPE=3",
	"CITER=10 CALPHA=0.05 NOPRIOR=1", 
	"ISAMPLE=1 ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1",
	"$EST METHOD=IMP  INTERACTION EONLY=1 NITER=2 ISAMPLE=300 PRINT=1 SIGL=10 NOPRIOR=1",
	"$EST METHOD=BAYES INTERACTION FILE=test7.txt NOTITLE=0 NOLABEL=1 NBURN=500 NITER=500 PRINT=100 CTYPE=3 CITER=10",
	"CALPHA=0.05 NOPRIOR=0",
	"$EST METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 NOABORT NOPRIOR=1",
	"FILE=test7.EXT NOLABEL=1" )

	estResult1 <- RNMImport:::.importNmModEst(estText)
	
	estMatrix1 <- cbind("method" = c("ITS", "SAEM", "IMP", "BAYES", "COND"), "file" = c("test7.EXT","","","test7.txt", "test7.EXT"),
			"noTitle" = c("1", "1", "1","0","0"), "noLabel" = c("0", "0", "0", "1", "1"),
			remainingText = c(" INTERACTION NITER=100 PRINT=5 NOABORT SIGL=8 CTYPE=3 CITER=10 NOPRIOR=1 CALPHA=0.05 NSIG=2",
					" INTERACTION NBURN=200 NITER=300 SIGL=8 ISAMPLE=2 PRINT=10 SEED=1556678 CTYPE=3 CITER=10 CALPHA=0.05 NOPRIOR=1 ISAMPLE=1 ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1",
					" INTERACTION EONLY=1 NITER=2 ISAMPLE=300 PRINT=1 SIGL=10 NOPRIOR=1",
					" INTERACTION NBURN=500 NITER=500 PRINT=100 CTYPE=3 CITER=10 CALPHA=0.05 NOPRIOR=0",
					" INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 NOABORT NOPRIOR=1 "
					))
	# check that the EST statement is parsed into multiple lines of text
	
	checkEquals( estResult1, estMatrix1, msg = " |EST statement parsed into matrix correctly" )
}

