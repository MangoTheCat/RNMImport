# $Rev: 115297 $
# $LastChangedDate: 2014-07-22 09:21:30 +0100 (Tue, 22 Jul 2014) $
# $LastChangedBy: jli@MANGO.LOCAL $
# Tests the show methods
# Author: fgochez
###############################################################################

# ** NOTE:**
# These tests only appear to work if executed from inside the Rgui using runRNMImportTests(...)
# they DO NOT work when run from ant test (thus Hudson), because the output dumped by the show methods
# there can have a different number of lines.
# TODO: Find some way around this.



# this is currently disabled

.test.show.NMRun <- function()
{
	
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	# TODO: check the initial chunk of text!
	
	# check the last 3 lines
	test1 <- capture.output(show(run1))
	
	expectedOut <- c("Number of problems: 1 ", "Problem statements:", "1 System Test 1 ")
	
	checkEquals(tail(test1, 3), expectedOut)
	

}

.test.show.NMBasicModel <- function()
{

	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun" ))
	
	prob1 <- getProblem(run1)
	test1 <- capture.output(show(prob1))
	
	expectedOut <- 
			c("Standard NONMEM problem:", "###############", "Problem statement:  System Test 1 ", 
					"Data file: Data1 ", "Input table dimensions:", "1061 19 ", "Input table columns:", 
					"SID SEX AGE RACE HT SMOK HCTZ PROP CON AMT WT TIME SECR DV EVID SS II ID OCC ", 
					"PK: ", "[1] \"TVCL=THETA(1) \"        \"TVV=THETA(2) \"         \"TVKA=THETA(3) \"        \"CL=TVCL*EXP(ETA(1) ) \" \"V =TVV *EXP(ETA(2) ) \" \"KA=TVKA*EXP(ETA(3) ) \"", 
					"[7] \"S2=V\"                 ", "", "Error:", "[1] \"IPRED = F\"                 \"IRES  = DV - F\"            \"W     = F\"                 \"IF(W.EQ.0) W = 1\"          \"IWRES = IRES/W\"           ", 
					"[6] \"Y     = IPRED + W*EPS(1) \"", "Parameter estimates:", 
					"###############", "THETAs:", "THETA1 THETA2 THETA3 ", " 19.60  84.60   1.66 ", 
					"OMEGAs:", "       OMEGA1 OMEGA2 OMEGA3", "OMEGA1  0.164  0.000    0.0", 
					"OMEGA2  0.000  0.165    0.0", "OMEGA3  0.000  0.000    1.3", 
					"SIGMAs:", "[1] 0.0202", "Output table files: sdtab1,patab1,cotab1,catab1,mutab1,mytab1 ", 
					"Output table dimensions:", "1061 24 ", "Output table columns:", 
					"ID TIME IPRED IWRES DV PRED RES WRES CL V KA AGE HT WT SECR SEX RACE SMOK HCTZ PROP CON OCC SID absWRES "
			)
	checkEquals(test1,	expectedOut, msg = " |show for NMBasicModel correct")
	
}

.test.show.NMSimModel <- function()
{

	run1 <- importNm( "TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun" ))
	
	prob1 <- getProblem(run1)
	test1 <- capture.output(show(prob1))
	
	expected1 <- c("NONMEM data simulation problem:", "###############", "Problem statement:  System Test 2 ", 
			"PK: ", "[1] \"TVCL=THETA(1) \"        \"TVV=THETA(2) \"         \"TVKA=THETA(3) \"        \"CL=TVCL*EXP(ETA(1) ) \" \"V =TVV *EXP(ETA(2) ) \" \"KA=TVKA*EXP(ETA(3) ) \"", 
			"[7] \"S2=V\"                 ", "", "Data file: Data1 ", "Input table dimensions:", 
			"1061 19 ", "Input table columns:", "SID SEX AGE RACE HT SMOK HCTZ PROP CON AMT WT TIME SECR DV EVID SS II ID OCC ", 
			"Output table files: sdtab1,patab1,cotab1,catab1,mutab1,mytab1 ", 
			"Output table dimensions:", "5305 24 ", "Output table columns:", 
			"ID TIME IPRED IWRES DV PRED RES WRES CL V KA AGE HT WT SECR SEX RACE SMOK HCTZ PROP CON OCC SID absWRES ", 
			"Number of simulations performed:  5 ", "Seeds:  20050213 ", 
			"Final estimates for each subproblem:", "", "THETAs:", "     THETA1 THETA2 THETA3", 
			"sim1   17.2  117.0   1.24", "sim2   18.3  108.0   1.40", "sim3   18.0  108.0   1.24", 
			"sim4   19.0   98.9   1.55", "sim5   17.1  109.0   1.38", "OMEGAs:", 
			", , sim1", "", "       OMEGA1 OMEGA2 OMEGA3", "OMEGA1  0.174   0.00   0.00", 
			"OMEGA2  0.000   0.19   0.00", "OMEGA3  0.000   0.00   1.45", 
			"", ", , sim2", "", "       OMEGA1 OMEGA2 OMEGA3", "OMEGA1  0.167  0.000   0.00", 
			"OMEGA2  0.000  0.143   0.00", "OMEGA3  0.000  0.000   1.24", 
			"", ", , sim3", "", "       OMEGA1 OMEGA2 OMEGA3", "OMEGA1  0.181  0.000   0.00", 
			"OMEGA2  0.000  0.142   0.00", "OMEGA3  0.000  0.000   1.57", 
			"", ", , sim4", "", "       OMEGA1 OMEGA2 OMEGA3", "OMEGA1  0.245  0.000  0.000", 
			"OMEGA2  0.000  0.189  0.000", "OMEGA3  0.000  0.000  0.945", 
			"", ", , sim5", "", "       OMEGA1 OMEGA2 OMEGA3", "OMEGA1  0.174  0.000   0.00", 
			"OMEGA2  0.000  0.197   0.00", "OMEGA3  0.000  0.000   1.47", 
			"", "SIGMAs:", ", , sim1", "", "       SIGMA1", "SIGMA1 0.0241", 
			"", ", , sim2", "", "       SIGMA1", "SIGMA1  0.026", "", ", , sim3", 
			"", "       SIGMA1", "SIGMA1 0.0262", "", ", , sim4", "", "       SIGMA1", 
			"SIGMA1 0.0295", "", ", , sim5", "", "       SIGMA1", "SIGMA1 0.0244", 
			"", "Objective function(s): ", "    sim1     sim2     sim3     sim4     sim5 ", 
			"3696.247 3575.252 3660.355 3606.526 3701.472 ")
	checkEquals(test1, expected1, msg = " | show for sim model correct\n")
	
}

.test.show.NMBasicNM7 <- function()
{
	run1 <- importNm( "TestData1.ctl", path = file.path(unitTestPath, "testdata/TestDataNM7" ))
	
	prob1 <- getProblem(run1)
	test1 <- capture.output(show(prob1))
	
	expected1 <- c("Standard NONMEM (7) problem:", "###############", "Problem statement:  NM7 test 1 ", 
			"Data file: Data1 ", "Input table dimensions:", "1061 19 ", "Input table columns:", 
			"SID SEX AGE RACE HT SMOK HCTZ PROP CON AMT WT TIME SECR DV EVID SS II ID OCC ", 
			"PK: ", "[1] \"TVCL=THETA(1) \"        \"TVV=THETA(2) \"         \"TVKA=THETA(3) \"        \"CL=TVCL*EXP(ETA(1) ) \" \"V =TVV *EXP(ETA(2) ) \" \"KA=TVKA*EXP(ETA(3) ) \"", 
			"[7] \"S2=V\"                 ", "", "Error:", "[1] \"IPRED = F\"                 \"IRES  = DV - F\"            \"W     = F\"                 \"IF(W.EQ.0) W = 1\"          \"IWRES = IRES/W\"           ", 
			"[6] \"Y     = IPRED + W*EPS(1) \"", "Methods used: ", "[1] \"Iterative Two Stage\"                               \"Stochastic Approximation Expectation-Maximization\"", 
			"Parameter estimates:", "###############", "THETAs:", "[[1]]", 
			"  TH1   TH2   TH3 ", "20.00 77.30  1.27 ", "", "[[2]]", "  TH1   TH2   TH3 ", 
			"19.10 76.70  1.68 ", "", "OMEGAs:", "[[1]]", "      ETA1  ETA2  ETA3", 
			"ETA1 0.157 0.000 0.000", "ETA2 0.000 0.162 0.000", "ETA3 0.000 0.000 0.737", 
			"", "[[2]]", "      ETA1  ETA2 ETA3", "ETA1 0.145 0.000 0.00", 
			"ETA2 0.000 0.149 0.00", "ETA3 0.000 0.000 1.42", "", "SIGMAs:", 
			"[[1]]", "       EPS1", "EPS1 0.0259", "", "[[2]]", "       EPS1", 
			"EPS1 0.0266", "", "Output table files: tab1 ", "Output table dimensions:", 
			"1061 11 ", "Output table columns:", "ID TIME IPRED IWRES WT ETA1 DV PRED RES WRES absWRES "
	)
	
	checkEquals(test1, expected1, msg = " | show for NMBasic NM7 correct")
	
}
