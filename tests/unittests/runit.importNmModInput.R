
## Test suite designed to test the importNmModInput function ##

test.importNmModInput <- function() {
  .importNmModInput <- RNMImport:::.importNmModInput
  test1.data  <- c("$INPUT ID AMT TIME DV=DROP DV=LNDV TAD DAY DOSE RATE CMT", 
    "EVID MDV=DROP PSNO=DROP PSCH WEEK=DROP OCC AGE=DROP", 
    "SEX RACE=DROP WGT CLCR BMI PER=DROP PDAY=DROP SEQ STUD")
  test1.expec <- matrix(c("ID","AMT","TIME","DV"  ,"DV",  "TAD","DAY","DOSE","RATE","CMT","EVID","MDV", "PSNO","PSCH","WEEK","OCC","AGE", "SEX","RACE","WGT","CLCR","BMI","PER", "PDAY","SEQ","STUD",
                          "ID","AMT","TIME","DROP","LNDV","TAD","DAY","DOSE","RATE","CMT","EVID","DROP","DROP","PSCH","DROP","OCC","DROP","SEX","DROP","WGT","CLCR","BMI","DROP","DROP","SEQ","STUD"), ncol=2)
  test1.out   <- .importNmModInput(test1.data); dimnames(test1.out) <- NULL
  checkEquals( test1.expec, test1.out, 
    msg = "check 1")
  
  test2.data  <- c("$INPUT ID TIME DV CMT EVID AMT RATE SCRT AGE SEX BW CLCR")  
  test2.expec <- matrix(
    c("ID","TIME","DV","CMT","EVID","AMT","RATE","SCRT","AGE","SEX","BW","CLCR",
      "ID","TIME","DV","CMT","EVID","AMT","RATE","SCRT","AGE","SEX","BW","CLCR"), ncol=2)
  test2.out   <- .importNmModInput(test2.data); dimnames(test2.out) <- NULL
  checkEquals( test2.expec , test2.out, 
    msg = "check 2")
  
  test3.data  <- c("$INPUT ID TIME CONC=DV AMT EVID RATE DUR MDV CYCL CMT TREL TDOS DOSE STDY=DROP","AGE WT HT BMI CLCR SEX=DROP RACE=DROP")
  test3.expec <- matrix(c("ID","TIME","CONC","AMT","EVID","RATE","DUR","MDV","CYCL","CMT","TREL","TDOS","DOSE","STDY","AGE","WT","HT","BMI","CLCR","SEX" ,"RACE",
                          "ID","TIME","DV"  ,"AMT","EVID","RATE","DUR","MDV","CYCL","CMT","TREL","TDOS","DOSE","DROP","AGE","WT","HT","BMI","CLCR","DROP","DROP"), ncol=2)
  test3.out   <- .importNmModInput(test3.data); dimnames(test3.out) <- NULL
  checkEquals( test3.expec , test3.out, 
    msg = "check 3")

  test4.data <- c("$INPUT ID DOSE AMT RATE CMT TIME DV EVID OCC WGT HT BSA AGE ")
  test4.expec <- matrix(c("ID","DOSE","AMT","RATE","CMT","TIME","DV","EVID","OCC","WGT","HT","BSA","AGE",
                          "ID","DOSE","AMT","RATE","CMT","TIME","DV","EVID","OCC","WGT","HT","BSA","AGE"), ncol=2)
  test4.out   <- .importNmModInput(test4.data); dimnames(test4.out) <- NULL
  checkEquals( test4.expec , test4.out, 
    msg = "check 4")

  test5.data  <- c("$INPUT ID TIME AMT DV DOSE ")
  test5.expec <- matrix(c("ID","TIME","AMT","DV","DOSE",
                          "ID","TIME","AMT","DV","DOSE"), ncol=2)
  test5.out   <- .importNmModInput(test5.data); dimnames(test5.out) <- NULL
  checkEquals( test5.expec , test5.out, 
    msg = "check 5")
  
  test6.data  <- c("$INPUT ID TIME AMT DV DOSE ")
  test6.expec <- matrix(c("ID","TIME","AMT","DV","DOSE",
                          "ID","TIME","AMT","DV","DOSE"), ncol=2)
  test6.out   <- .importNmModInput(test6.data); dimnames(test6.out) <- NULL
  checkEquals( test6.expec , test6.out, 
    msg = "check 6")
  
  test7.data  <- c("$INPUT   ID TIME AMT DV DOSE ")
  test7.expec <- matrix(c("ID","TIME","AMT","DV","DOSE",
                          "ID","TIME","AMT","DV","DOSE"), ncol=2)
  test7.out   <- .importNmModInput(test7.data); dimnames(test7.out) <- NULL
  checkEquals( test7.expec , test7.out, 
    msg = "check 7")
  
	test8.data <- c("$INPUT ID TIME DV CMT EVID AMT RATE SCRT AGE SEX BW CLCR ")
  test8.expec <- matrix(c("ID","TIME","DV","CMT","EVID","AMT","RATE","SCRT","AGE","SEX","BW","CLCR",
                          "ID","TIME","DV","CMT","EVID","AMT","RATE","SCRT","AGE","SEX","BW","CLCR"), ncol=2)
  test8.out   <- .importNmModInput(test8.data); dimnames(test8.out) <- NULL
  checkEquals( test8.expec , test8.out, 
    msg = "check 8")
	
	
# additional tests that import entire runs	
}



