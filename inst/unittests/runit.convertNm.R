
## Test suite designed to test the convertNm function ##

# originally by R Francois

test.convertNm <- function() {
  chop <- function(x) gsub("[[:space:]]+", "", x)
  
	myPred <- c("BASE=THETA(1)*EXP(ETA(1))", 
	" C50=THETA(2)*EXP(ETA(2))", 
	"GAM=THETA(3)*EXP(ETA(3))", 
	"IF (GAM.EQ.0) EXIT 1", 
	"IF (GAM.GT.10) EXIT 1 ", 
	"EMAX1=LOG(THETA(4)/(1-THETA(4)) )",
	"EMAX=BASE*EXP(EMAX1+ETA(4))/(1+EXP(EMAX1+ETA(4)))", 
	"F=BASE-EMAX*(CONC)**GAM/((CONC)**GAM+(C50+0.001)**GAM)", 
	" Y=F+ERR(1) ", 
	"IPRED=F",   
	"IRES=DV-IPRED", 
	"IWRES=1 ", 
	"A=MIXNUM", # Testing mixture models
	"B=NEWIND") # Testing identification of "NEWIND" variable
	expResult1 <- c("NEWIND <- makeNewindColumn(ID)", "BASE <- TH1 * exp(ETA1)", "C50 <- TH2 * exp(ETA2)", "GAM <- TH3 * exp(ETA3)", "EMAX1 <- log(TH4 /(1 - TH4))", "EMAX <- BASE * exp(EMAX1 + ETA4)/(1 + exp(EMAX1 + ETA4))", "FX <- BASE - EMAX *(CONC)^GAM /((CONC)^GAM +(C50 + 0.001)^GAM)",
		"Y <- FX + EPS1", "IPRED <- FX", "IRES <- DV - IPRED", "IWRES <- 1", "A <- MIXNUM", "B <- NEWIND" )
	expResult2 <- c("P1 <- TH1", "P2 <- 1 - TH1", "MIXNUM <- makeMixNumColumn(ID , TRIAL , P1, P2)", expResult1)

	checkEquals( chop(expResult1), chop(RNMImport:::convertNm(myPred))      )
	checkEquals( chop(expResult2), chop(RNMImport:::convertNm(myPred, mix=c("NSPOP=2", "P(1) = THETA(1)", "P(2) = 1 - THETA(1)"))))

}

