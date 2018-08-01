
## Test suite designed to test the importNmModSim function ##

test.importNmModSim <- function() {

	out1 <- RNMImport:::.importNmModSim("  (11 UNIFORM) (22 NORMAL) NSUB = 20")
  checkEquals( out1[["nSub"  ]] , "20"          )
  checkEquals( out1[["Seed1" ]] , "11 UNIFORM" )
  checkEquals( out1[["Seed2" ]] , "22 NORMAL"  )
	
  out2 <- RNMImport:::.importNmModSim("  (11) (22)  NSUB = 20")
  checkEquals( out2[[ "nSub" ]] , "20")
  checkEquals( out2[[ "Seed1"]] , "11")
  checkEquals( out2[[ "Seed2"]] , "22")
	
  out3 <- RNMImport:::.importNmModSim("  (11) (22) NSUBPROBS = 20")
  checkEquals( out3[["nSub"  ]]  , "20")
  checkEquals( out3[["Seed1" ]]  , "11")
  checkEquals( out3[["Seed2" ]]  , "22")
	                                                   
  out4 <- RNMImport:::.importNmModSim("  (11) (22)  SUBPROBS = 20")
  checkEquals( out4[["nSub"  ]] , "20" )
  checkEquals( out4[["Seed1" ]] , "11" )
  checkEquals( out4[["Seed2" ]] , "22" )
  
	out5 <- RNMImport:::.importNmModSim("  (11) (22)  NSUBPROBLEMS = 20")
  checkEquals( out5[["nSub"  ]] , "20" )
  checkEquals( out5[["Seed1" ]] ,"11" )
  checkEquals( out5[["Seed2" ]] ,"22" )

	out6 <- RNMImport:::.importNmModSim("  (11) (22)  SUBPROBLEMS = 20")
  checkEquals( out6[["nSub"  ]] , "20" )
  checkEquals( out6[["Seed1" ]] ,"11" )
  checkEquals( out6[["Seed2" ]] ,"22" )

	out7 <- RNMImport:::.importNmModSim(" ONLYSIMULATION (11) (22)  SUBPROBLEMS = 20 OMITTED REQUESTFIRST REQUESTSECOND PREDICTION NOPREDICTION")
  checkEquals( out7[["nSub"  ]] , "20" )
  checkEquals( out7[["Seed1" ]] , "11" )
  checkEquals( out7[["Seed2" ]] , "22" )                    

} 