
## Test suite designed to test the .importNmModOmega function ##

test.importNmModOmega <- function(){

	nonames <- RNMImport:::nonames
	nonames.array <- RNMImport:::nonames.array
	nonames.default <- RNMImport:::nonames.default
	nonames.matrix <- RNMImport:::nonames.matrix
	checkEquals( RNMImport:::.importNmModOmega( "0.02" ), 
    matrix(0.02, dimnames = rep(list("OMEGA1"), 2) ) , msg = "single object")
	
  checkEquals( RNMImport:::.importNmModOmega( " 0.02 FIXED "),  
    matrix(0.02, dimnames = rep(list("OMEGA1"), 2) ) , msg = "single object, fixed ")
    
	out3 <- RNMImport:::.importNmModOmega( c(" 0.2 0.2 0.15  ") )
  checkEquals( as.vector(rownames(out3)), sprintf("OMEGA%d", 1:3), msg = "dimnames" )
  checkEquals( nonames(out3), diag(  c( 0.2, 0.2, 0.15) ) , msg = "diag")
  
	out4 <- RNMImport:::.importNmModOmega( c("BLOCK (1) 0.0102 ","BLOCK (1)  0.0147 ","BLOCK (1)  0.0212 "))
  expec4 <- cbind( c(0.0102,0,0) , c(0,0.0147,0) , c(0,0,0.0212) ) 
	checkEquals( rownames(out4), sprintf("OMEGA%d", 1:3), msg = "dimnames" )
  checkEquals( nonames(out4), diag(  c( 0.0102, 0.0147, 0.0212)  ) , msg = "diag")
  
  out5 <- RNMImport:::.importNmModOmega( 
     c("0.15      ",
       "0.75      ",
       "1  FIXED  ",
       "BLOCK(1) 0.037   ",
       "BLOCK(1) SAME    ",
       "BLOCK(1) SAME    ",
       "BLOCK(1) 0.21    ",
       "BLOCK(1) SAME    ",
       "BLOCK(1) SAME    ",
       "0.5      "))       
  expec5 <- cbind( c(0.15,0,0,0,0,0,0,0,0,0) , 
                   c(0,0.75,0,0,0,0,0,0,0,0) , 
                   c(0,0,1,0,0,0,0,0,0,0) , 
                   c(0,0,0,0.037,0,0,0,0,0,0) , 
                   c(0,0,0,0,0.037,0,0,0,0,0) , 
                   c(0,0,0,0,0,0.037,0,0,0,0) , 
                   c(0,0,0,0,0,0,0.21,0,0,0) , 
                   c(0,0,0,0,0,0,0,0.21,0,0) , 
                   c(0,0,0,0,0,0,0,0,0.21,0) , 
                   c(0,0,0,0,0,0,0,0,0,0.5) )
  checkEquals( rownames(out5), sprintf("OMEGA%d", 1:10), msg = "dimnames" )
  dimnames(out5) <- NULL
  checkEquals( out5, expec5 )
  
	out6 <- RNMImport:::.importNmModOmega( c("BLOCK (1)  0.0102 ","BLOCK (1)  0.0147 ","BLOCK (1)  0.0212 ")) 
  exp6 <- cbind( c(0.0102,0,0) , c(0,0.0147,0) , c(0,0,0.0212) ) 
  dimnames(out6) <- NULL
  checkEquals( out6, exp6, msg = "1-size blocks" )
  
	out7 <- RNMImport:::.importNmModOmega( c("BLOCK(2) 0.3 0.05 0.3    ","         0.3         ")) 
  exp7 <- cbind( c(0.3,0.05,0) , c(0.05,0.3,0) , c(0,0,0.3) )
  dimnames(out7) <- NULL
  checkEquals( out7, exp7, msg = "2-size block" )
  
  out8 <- RNMImport:::.importNmModOmega( c("BLOCK (1) 0.0102 ","BLOCK (1) 0.0147 ","BLOCK (1) 0.0212 "))
  exp8 <- cbind( c(0.0102,0,0) , c(0,0.0147,0) , c(0,0,0.0212) )
  dimnames( out8) <- NULL
  checkEquals( out8, exp8 , msg = "1-size block")
  
	out9 <- RNMImport:::.importNmModOmega( 
    c("BLOCK(4)  0.2  0.01  0.2  0.001 0.001 0.2    0  0.001 0.01  0.2   ")) 
  exp9 <- cbind(
    c(0.2,0.01,0.001,0) , 
    c(0.01,0.2,0.001,0.001) , 
    c(0.001,0.001,0.2,0.01) , 
    c(0,0.001,0.01,0.2) )
  dimnames( out9) <- NULL
  checkEquals( out9, exp9 , msg = "4-size block")
    
	out10 <- RNMImport:::.importNmModOmega( c(
    "0.5               ",
    "0.5               ",
    "0.5               ",
    "0.5               ",
    "0.5               ",
    "0.5               ",
    "2                 ",
    "BLOCK(1) 0.05     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ",
    "BLOCK(1) SAME     ")) 
  exp10 <- cbind( 
    c(0.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) , 
    c(0,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) , 
    c(0,0,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0,0) ,
    c(0,0,0,0.5,0,0,0,0,0,0,0,0,0,0,0,0,0) , 
    c(0,0,0,0,0.5,0,0,0,0,0,0,0,0,0,0,0,0) , 
    c(0,0,0,0,0,0.5,0,0,0,0,0,0,0,0,0,0,0) , 
    c(0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0) ,
    c(0,0,0,0,0,0,0,0.05,0,0,0,0,0,0,0,0,0) , 
    c(0,0,0,0,0,0,0,0,0.05,0,0,0,0,0,0,0,0) ,
    c(0,0,0,0,0,0,0,0,0,0.05,0,0,0,0,0,0,0) ,
    c(0,0,0,0,0,0,0,0,0,0,0.05,0,0,0,0,0,0) ,
    c(0,0,0,0,0,0,0,0,0,0,0,0.05,0,0,0,0,0) ,
    c(0,0,0,0,0,0,0,0,0,0,0,0,0.05,0,0,0,0) , 
    c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.05,0,0,0) , 
    c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05,0,0) , 
    c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05,0) , 
    c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.05) )
  dimnames( out10) <- NULL
  checkEquals( out10, exp10 , msg = "same")
  
	out11 <- RNMImport:::.importNmModOmega( 
    c(" 0.0158 "," 0.0238"," 0 FIX"," 0 FIX "," 0 FIX"," 0 FIX"," 0.00076 "," 0.00076  "," 0.00076  "))
  exp11 <- cbind( c(0.0158,0,0,0,0,0,0,0,0) , 
                  c(0,0.0238,0,0,0,0,0,0,0) , 
                  c(0,0,0,0,0,0,0,0,0) ,
                  c(0,0,0,0,0,0,0,0,0) , 
                  c(0,0,0,0,0,0,0,0,0) ,
                  c(0,0,0,0,0,0,0,0,0) ,
                  c(0,0,0,0,0,0,0.00076,0,0) , 
                  c(0,0,0,0,0,0,0,0.00076,0) , 
                  c(0,0,0,0,0,0,0,0,0.00076) )
  dimnames( out11) <- NULL
  checkEquals( out11, exp11 , msg = "fix")
  
	out12 <- RNMImport:::.importNmModOmega(
    c("DIAG(1) 0.2 ",
      "BLOCK(2) 4 0.02 0.03 ",
      "BLOCK(2) 0.5 0.02 0.3 ",
      "DIAG(2) 0.03 0.1 ",
      "BLOCK(1) 4.6 ",
      "BLOCK(1) SAME ",
      "BLOCK(1) SAME ")) 
  exp12 <- cbind( 
    c(0.2,0,0,0,0,0,0,0,0,0) , 
    c(0,4,0.02,0,0,0,0,0,0,0) , 
    c(0,0.02,0.03,0,0,0,0,0,0,0) , 
    c(0,0,0,0.5,0.02,0,0,0,0,0) , 
    c(0,0,0,0.02,0.3,0,0,0,0,0) , 
    c(0,0,0,0,0,0.03,0,0,0,0) , 
    c(0,0,0,0,0,0,0.1,0,0,0) ,
    c(0,0,0,0,0,0,0,4.6,0,0) , 
    c(0,0,0,0,0,0,0,0,4.6,0) , 
    c(0,0,0,0,0,0,0,0,0,4.6) )
  dimnames( out12 ) <- NULL
  checkEquals( out12, exp12 , msg = "SAME")
  
		
}



