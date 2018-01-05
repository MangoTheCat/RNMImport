
## Test suite designed to test the importNmLstIter function ##

test.importNmLstIter <- function() {
	ColiNum <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	ColIter <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 10, 10, 10, 10, 10, 10, 10, 10, 10, 15, 15, 15, 15, 15, 15, 15, 15, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 25, 25, 25, 25, 25, 25, 25, 25, 25)
	ColObjective <- c(1627, 1627, 1627, 1627, 1627, 1627, 1627, 1627, 1627, 1417, 1417, 1417, 1417, 1417, 1417, 1417, 1417, 1417, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256)
	ColfEvals <- c(8, 8, 8, 8, 8, 8, 8, 8, 8, 10, 10, 10, 10, 10, 10, 10, 10, 10, 12, 12, 12, 12, 12, 12, 12, 12, 12, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0)
	ColParameter <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1775, 0.07445, 0.1561, -0.05977, 0.1436, 0.1476, 0.2238, 0.137, 0.1177, 0.1461, 0.08473, 0.1326, -0.05791, 0.07367, 0.1326, 0.1593, 0.1385, 0.1194, 0.1102, 0.07073, 0.1025, -0.05159, 0.08158, 0.1084, 0.09748, 0.08955, -0.002892, 0.1084, 0.06972, 0.1005, -0.05258, 0.06903, 0.1081, 0.09847, 0.06486, 0.0004026, 0.1086, 0.0694, 0.1008, -0.05252, 0.06874, 0.1083, 0.09861, 0.06499, -9.799e-005)
	ColGradient <- c(5646, 1146, 528.9, 14610, -3906, -12270, -274.5, 251.2, -132, 1881, -146.4, 3247, -70.47, 103.4, -747.9, -132.8, -1349, 7.197, 2459, 136.4, 729.8, -283.8, -52.19, -978.3, -19.69, -342.9, 62.09, 178.6, 46.26, -587.7, 456.9, 90.58, 81.63, -11.77, 245.6, -3.581, -15.44, 16.57, -40.42, 13.17, 2.233, 8.229, -0.99, 8.34, 0.4648, -6.126, -0.05194, -3.713, -6.258, -0.04065, -2.538, 0.5147, -0.2131, -0.1142)
	
	myInput <- list( 
          c("START PADDING 1", "START PADDING 2", "START PADDING 3" ),
  				c("ITERATION NO.:    0     OBJECTIVE VALUE:  0.1627E+04     NO. OF FUNC. EVALS.: 8", 
					"CUMULATIVE NO. OF FUNC. EVALS.:    8", 
					"PARAMETER:  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00", 
					"GRADIENT:   0.5646E+04  0.1146E+04  0.5289E+03  0.1461E+05 -0.3906E+04 -0.1227E+05 -0.2745E+03  0.2512E+03 -0.1320E+03" ), 
					c("ITERATION NO.:    5     OBJECTIVE VALUE:  0.1417E+04     NO. OF FUNC. EVALS.:10", 
					"CUMULATIVE NO. OF FUNC. EVALS.:   56", 
					"PARAMETER:  0.1775E+00  0.7445E-01  0.1561E+00 -0.5977E-01  0.1436E+00  0.1476E+00  0.2238E+00  0.1370E+00  0.1177E+00", 
					"GRADIENT:   0.1881E+04 -0.1464E+03  0.3247E+04 -0.7047E+02  0.1034E+03 -0.7479E+03 -0.1328E+03 -0.1349E+04  0.7197E+01"), 
					c("ITERATION NO.:   10     OBJECTIVE VALUE:  0.1317E+04     NO. OF FUNC. EVALS.:12", 
					"CUMULATIVE NO. OF FUNC. EVALS.:  111", 
					"PARAMETER:  0.1461E+00  0.8473E-01  0.1326E+00 -0.5791E-01  0.7367E-01  0.1326E+00  0.1593E+00  0.1385E+00  0.1194E+00", 
					"GRADIENT:   0.2459E+04  0.1364E+03  0.7298E+03 -0.2838E+03 -0.5219E+02 -0.9783E+03 -0.1969E+02 -0.3429E+03  0.6209E+02"), 
					c("ITERATION NO.:   15     OBJECTIVE VALUE:  0.1260E+04     NO. OF FUNC. EVALS.: 9", 
					"CUMULATIVE NO. OF FUNC. EVALS.:  156", 
					"PARAMETER:  0.1102E+00  0.7073E-01  0.1025E+00 -0.5159E-01  0.8158E-01  0.1084E+00  0.9748E-01  0.8955E-01 -0.2892E-02", 
					"GRADIENT:   0.1786E+03  0.4626E+02 -0.5877E+03  0.4569E+03  0.9058E+02  0.8163E+02 -0.1177E+02  0.2456E+03 -0.3581E+01"), 
					c("ITERATION NO.:   20     OBJECTIVE VALUE:  0.1256E+04     NO. OF FUNC. EVALS.: 9", 
					"CUMULATIVE NO. OF FUNC. EVALS.:  201", 
					"PARAMETER:  0.1084E+00  0.6972E-01  0.1005E+00 -0.5258E-01  0.6903E-01  0.1081E+00  0.9847E-01  0.6486E-01  0.4026E-03", 
					"GRADIENT:  -0.1544E+02  0.1657E+02 -0.4042E+02  0.1317E+02  0.2233E+01  0.8229E+01 -0.9900E+00  0.8340E+01  0.4648E+00"), 
					c("ITERATION NO.:   25     OBJECTIVE VALUE:  0.1256E+04     NO. OF FUNC. EVALS.: 0", 
					"CUMULATIVE NO. OF FUNC. EVALS.:  247", 
					"PARAMETER:  0.1086E+00  0.6940E-01  0.1008E+00 -0.5252E-01  0.6874E-01  0.1083E+00  0.9861E-01  0.6499E-01 -0.9799E-04", 
					"GRADIENT:  -0.6126E+01 -0.5194E-01 -0.3713E+01 -0.6258E+01 -0.4065E-01 -0.2538E+01  0.5147E+00 -0.2131E+00 -0.1142E+00"),
					c("END PADDING 1", "END PADDING 2") )

	myDf <- RNMImport:::.importNmLstIter(myInput)	
	checkTrue( length(myDf$paramNum) == length(ColiNum) && all(myDf$paramNum == ColiNum)  )
	checkTrue( length(myDf$iterationNum) == length(ColIter) && all(myDf$iterationNum== ColIter)   )
	checkTrue( length(myDf$objective) == length(ColObjective) && all(myDf$objective== ColObjective)  )
	checkTrue( length(myDf$numFuncEvals) == length(ColfEvals) && all(myDf$numFuncEvals== ColfEvals)       )
	checkTrue( length(myDf$parameterVal) == length(ColParameter) && all(myDf$parameterVal == ColParameter) )
	checkTrue( length(myDf$gradient) == length(ColGradient) && all(myDf$gradient == ColGradient) )


}
