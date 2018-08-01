# $LastChangedDate: 2009-12-16 14:27:59 +0000 (Wed, 16 Dec 2009) $
# $LastChangedBy: fgochez $
# $Rev: 14119 $
# 
# Author: fgochez
###############################################################################

# tests the nmTitle utility function, found in lstparseutiles.R

test.nmTitle <- function()
{
	testSection1 <- c(
		" ************************************************************************************************************************", 
	 	" ********************                                                                                ********************",
		" ********************                                  FINAL PARAMETER ESTIMATE                      ********************", 
		" ********************                                                                                ********************", 
		" ************************************************************************************************************************",
		" THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********",                                                                  
		"            TH 1      TH 2      TH 3      TH 4",                                                                           
		"         2.78E+00  3.02E-01  4.90E-01  1.14E+01",                                                                        
		" OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********",                                                                  
		"            ETA1      ETA2      ETA3      ETA4",                                                                           
		" ETA1",                                                                                                                    
		"+        1.69E-01",                                                                                                        
		" ETA2",                                                                                                                    
 		"+        0.00E+00  1.08E-02",                                                                                              
		" ETA3",                                                                                                                    
		"+        0.00E+00  0.00E+00  9.42E-02",                                                                                    
		" ETA4",                                                                                                                    
		"+        0.00E+00  0.00E+00  0.00E+00  9.87E-02",                                                                          
		" SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****",                                                                  
		"            EPS1",                                                                                                         
		" EPS1",                                                                                                                    
		"+        1.97E-02"  )

	testSection1Split <- RNMImport:::nmTitle(testSection1)
	checkEquals( testSection1Split$text, testSection1[-(1:5)], msg =  " |text field should be the original text minus the header" )
	checkEquals( testSection1Split$title, "FINAL PARAMETER ESTIMATE", msg = " |title field is the text in the middle of a soup of stars")

}

# tests the nmVersion function

test.nmVersion <- function()
{
	testText1 <- c("#1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM)    DOUBLE PRECISION NONMEM    VERSION VI LEVEL 1.0",  
 				   "DEVELOPED AND PROGRAMMED BY STUART BEAL AND LEWIS SHEINER",
				   "PROBLEM NO.:         1")
	
		  
	versionInfo <- RNMImport:::nmVersion(testText1)
	checkEquals(versionInfo, c("Version" = "VI", "Level" = "1.0"), msg = " |Version info extracted correctly")
	errorCheck <- try(RNMImport:::nmVersion(testText1[-1]), silent = TRUE)
	checkTrue(length(grep(errorCheck, pattern = "Error : Could not find any version information in the contents of the report file")) > 0,
			msg = " |appropriate error text appears in the error message")
	
}