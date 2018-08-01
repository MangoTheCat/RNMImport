
## Test suite designed to test the importNmLstVector function ##

test.importNmLstVector <- function() {
	
	vec1 <- c("               TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8", 
            "        6.63E+00  1.26E+00  7.43E+01  2.61E+02  4.46E-02  7.51E-02  2.14E-01 -1.65E-01")
	result1 <- RNMImport:::.importNmLstVector(vec1)
	checkTrue( all(names(result1) == paste("TH", 1:8, sep="")), msg = "check1 (1)" )
  checkTrue( all(result1 ==  c(6.63,1.26,74.3,261,0.0446,0.0751,0.214,-0.165)),  msg = "check 1" )

	vec2 <- c("           TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      TH10      TH11      TH12",  
            "        5.80E+00  9.64E+00  9.40E-01  1.45E+00  0.00E+00  1.00E+00  1.00E+00  1.00E+00  0.00E+00  0.00E+00  0.00E+00  3.66E-01")
	result2 <- RNMImport:::.importNmLstVector(vec2)
	checkTrue( all(names(result2) == paste("TH", 1:12, sep="")), msg = "check 2 (1)" )
  checkTrue( all(result2 ==  c(5.8, 9.64, 0.94, 1.45, 0, 1, 1, 1, 0, 0, 0, 0.366)), msg = "check 2 (2)" )

	vec3 <- c("           TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      TH10      TH11      TH12", 
           "       .........  2.94E-01 ......... ......... .........  2.19E-01 ......... ......... ......... ......... .........  7.36E-01")
	result3 <- RNMImport:::.importNmLstVector(vec3)
	checkTrue( all(names(result3) == paste("TH", 1:12, sep="")) , msg = "check 3 (1)")
  checkTrue( all(result3 ==  c(0, 0.294, 0, 0, 0, 0.219, 0, 0, 0, 0, 0, 0.736)), msg = "check 3 (2)" )
		
}
