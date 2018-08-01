
## Test suite designed to test the importNmModTheta function ##
# Taken from the original RNONMEM
test.importNmModTheta <- function(){
	nonames <- RNMImport:::nonames
	nonames.array <- RNMImport:::nonames.array
	nonames.default <- RNMImport:::nonames.default
	nonames.matrix <- RNMImport:::nonames.matrix
	.importNmModTheta <- RNMImport:::.importNmModTheta
	if(!exists(".importNmModTheta", envir = .GlobalEnv))
		.importNmModTheta <- RNMImport:::.importNmModTheta
	input1 <- c(
			"$THETA           ",
			"(2,210,1000)     ",
			"(.01,.18,2)      ",
			"(.001,.05,2)     ",
			"(500,3000,6000)  ",
			"(0.1)            ")
	output1 <- RNMImport:::.importNmModTheta(input1)
	checkEquals( c(2, 1e-002, 1e-003, 5e+002, -Inf), nonames( output1[, "Lower"] ) )
	checkEquals( c(210, .18, .05, 3000, .1)        , nonames( output1[, "Est"  ] ) )
	checkEquals( c(1000, 2, 2, 6000, Inf)          , nonames( output1[, "Upper"] ) )
	
	input2 <- c(
			"$THETA         ",
			" (0,0.3)       ",
			" (0,1)         ",
			" (0,0.02)      ",
			" (0,2.5)       ")
	output2 <- RNMImport:::.importNmModTheta(input2)
	checkEquals( nonames( output2[ , "Lower"] ) , rep(0, 4) )
	checkEquals( nonames( output2[ , "Est"  ] ) , c(.3, 1, .02, 2.5) )
	checkEquals( nonames( output2[ , "Upper"] ) , rep(Inf, 4) )
	
	input3 <- c("$THETA", "(0,95,100)", "(0.001,10,20)", "(1,3,4)", "(0,0.98,0.99) ")
	output3 <- RNMImport:::.importNmModTheta(input3)
	checkEquals( c(0, .001, 1, 0)  , nonames( output3[ , "Lower"] ) )
	checkEquals( c(95, 10, 3, .98) , nonames( output3[ , "Est"]   ) )
	checkEquals( c(100, 20, 4, .99), nonames( output3[ , "Upper"] ) )
	
	input4 <- c(
			"$THETA  (0,0.29,2.4)         ",
			"$THETA  (0,0.7,5.8)          ",
			"$THETA  (0,4.1,33)           ",
			"$THETA  (0,3.8,32)           ",
			"$THETA  (FIX 2.47)           ",
			"$THETA  (FIX 8.33)           ",
			"$THETA  (0,2.7,22)           ",
			"$THETA  (0,0.19,1.6)         ")
	output4 <- RNMImport:::.importNmModTheta(input4)
	checkEquals( c(0  , 0  , 0  , 0  , 2.47, 8.33, 0  , 0  ) , nonames( output4[ , "Lower"] ) )
	checkEquals( c(.29, .7 , 4.1, 3.8, 2.47, 8.33, 2.7, .19) , nonames( output4[ , "Est"]   ) )
	checkEquals( c(2.4, 5.8, 33 , 32 , 2.47, 8.33, 22 , 1.6) , nonames( output4[ , "Upper"] ) )
	
	input5 <- c("$THETA", " (0,50,100)      ", " (0,250,500)     ", " (0,50,100)      ",
			" (0,650,1000)    ", " (0,0.6,10)       ", " (0,2.55,10)     ", " (-1,-0.35)    ",
			" (-1,-0.29)    ", " (-1,0.30)    ")
	output5 <- RNMImport:::.importNmModTheta(input5)
	checkEquals( c(0  , 0  , 0  , 0   , 0 , 0   , -1  , -1  , -1), nonames( output5[ , "Lower"] ) )
	checkEquals( c(50 , 250, 50 , 650 , .6, 2.55, -.35, -.29, .3), nonames( output5[ , "Est"]   ) )
	checkEquals( c(100, 500, 100, 1000, 10, 10  , Inf ,Inf  ,Inf), nonames( output5[ , "Upper"] ) )
	
	input6 <- c(
			"$THETA",
			" 5.8 FIXED                ",
			" (0,9.4)                  ",
			" 0.94 FIXED               ",
			" 1.45 FIXED               ",
			"0 FIXED                   ",
			"0.1                       ",
			"1 FIXED                   ",
			"1 FIXED                   ",
			"0 FIXED                   ",
			"0 FIXED                   ",
			"0 FIXED                   ",
			"0.1                  ")
	output6 <- RNMImport:::.importNmModTheta(input6)
	checkEquals( c(5.8, 0  , .94, 1.45, 0, -Inf, 1, 1, 0, 0, 0, -Inf), nonames( output6[ , "Lower"] ) )
	checkEquals( c(5.8, 9.4, .94, 1.45, 0, .1  , 1, 1, 0, 0, 0, .1  ), nonames( output6[ , "Est"]   ) )
	checkEquals( c(5.8, Inf, .94, 1.45, 0, Inf , 1, 1, 0, 0, 0, Inf ), nonames( output6[ , "Upper"] ) )
	
	input7 <- c(
			"$THETA  (0,0.29,2.4)       ;  ~X1",
			"$THETA  (0,0.7,5.8)        ;  ~X2",
			"$THETA  (0,4.1,33)         ;  ~X3",
			"$THETA  (0,3.8,32)         ;  ~X4",
			"$THETA  (FIX 2.47)         ;  ~X5",
			"$THETA  (FIX 8.33)         ;  ~X6",
			"$THETA  (0,2.7,22)           ",
			"$THETA  (0,0.19,1.6)       ;  ~X9")
	output7 <- RNMImport:::.importNmModTheta( input7 )
	n7 <- rownames(output7)
	checkEquals( n7[1], "X1")
	checkEquals( n7[2], "X2")
	checkEquals( n7[3], "X3")
	checkEquals( n7[4], "X4")
	checkEquals( n7[5], "X5")
	checkEquals( n7[6], "X6")
	checkEquals( n7[7], "THETA7")
	checkEquals( n7[8], "X9")
	
	input8 <- c(
			"$THETA  (0,0.29,2.4)         ; # X1",
			"$THETA  (0,0.7,5.8)          ; # X2",
			"$THETA  (0,4.1,33)           ; # X3",
			"$THETA  (0,3.8,32)           ; # X4",
			"$THETA  (FIX 2.47)           ; # X5",
			"$THETA  (FIX 8.33)           ; # X6",
			"$THETA  (0,2.7,22)           ",
			"$THETA  (0,0.19,1.6)         ;~ X9")
	output8 <- RNMImport:::.importNmModTheta( input8, rx = "#[[:space:]]*([[:alnum:]]*)" )
	n8 <- rownames(output8)
	checkEquals( n8[1], "X1")
	checkEquals( n8[2], "X2")
	checkEquals( n8[3], "X3")
	checkEquals( n8[4], "X4")
	checkEquals( n8[5], "X5")
	checkEquals( n8[6], "X6")
	checkEquals( n8[7], "THETA7")
	checkEquals( n8[8], "THETA8")
	
	input9 <- c("$THETA (9,11,13) (0,0.01,0.1)            ",
			"     (0,0.4,2) (0,0.06,0.1) (0,40,90)    ",
			"     (0,0.20,0.3) (0,0.30,0.5) (0,0.60,1)",
			"     (9,11,13) (9,12,13)                 ")
	output9 <- RNMImport:::.importNmModTheta( input9 )
	checkEquals( nonames(output9[,1]), c(9,0,0,0,0,0,0,0,9,9), msg = "several (,,) in one line" )
	checkEquals( nonames(output9[,2]), c(11.00,0.01,0.40,0.06,40.00,0.20,0.30,0.60,11.00,12.00) )
	checkEquals( nonames(output9[,3]), c(13.0,0.1,2.0,0.1,90.0,0.3,0.5,1.0,13.0,13.0) )
	
	input10 <- "$THTA (.4,1.7,7) (.025,.102,.4) (10,29,80)"
	#Lower    Est Upper
	#THETA1  0.400  1.700   7.0
	#THETA2  0.025  0.102   0.4
	#THETA3 10.000 29.000  80.0
	output10 <- nonames(.importNmModTheta(input10))
	expected <- rbind( c(.4,1.7,7), c(.025,.102,.4), c(10,29,80) )
	checkEquals(output10, expected, msg = "Theta specified by THTA")
	
	input11 <-  c("$THETA (0 90 480)   ;  PCB",  "$THETA (1 FIXED)    ; EMAX", 
			"$THETA (0 50 500)   ; EC50", "$THETA (0 0.5 1)         ; ZOLPIDEM")
	output11 <- RNMImport:::.importNmModTheta(input11)
	
	expected <- cbind("Lower" = c(0,1,0,0), "Est" = c(90,1,50,0.5), "Upper" = c(480, 1, 500, 1))
	rownames(expected) <- c("PCB", "EMAX", "EC50", "ZOLPIDEM")
	
	checkEquals(output11, expected)
	
	input12 <- "$THETA 4.3 -2.9 4.3 -0.67 (0.000001 0.667 0.9999)"
	output12 <-  RNMImport:::.importNmModTheta(input12)
	expected <- cbind( "Lower" = c(-Inf, -Inf, -Inf, -Inf, 1e-06),
			"Est" = c(4.3, -2.9, 4.3, -0.670, 0.667), 
			"Upper" = c(Inf, Inf, Inf, Inf, 0.9999) )
	rownames(expected) <- paste("THETA", 1:5, sep = "")
	checkEquals(expected, output12)
	
	input13 <- c("$THETA",	"(-1000.0  4.3 1000.0)",
	"(-1000.0 -2.9 1000.0)", "(-1000.0 -0.67 1000.0)", 
	"(0.0001 0.667 0.9999)")
	output13 <- RNMImport:::.importNmModTheta(input13)
	expected <- cbind(Lower = c(-1000, -1000, -1000, 0.0001 ), 
			Est = c(4.3, -2.9, -0.67, 0.667), Upper = c(1000, 1000, 1000, 0.9999))
	rownames(expected) <- paste("THETA", 1:4, sep = "")
	
	checkEquals(output13, expected)
}

