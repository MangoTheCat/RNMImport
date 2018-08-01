
## Test suite designed to test the importNmModTables function ##

test.importNmModTables <- function() {
	# .importNmModTables <- RNMImport:::.importNmModTables
  vec1 <- RNMImport:::.importNmModTables(
      c("$TABLE ID TIME DV CMT EVID AMT RATE CL V1 K K12 K21",
        "SCRT AGE SEX BW CLCR IPRED PRED WRES IRES IWRES",
        "ONEHEADER NOPRINT FILE=CBNCCV03.fit "))
	checkEquals(vec1[["File"]]    , "CBNCCV03.fit", msg = "check 1 (file)")
  checkEquals(vec1[["Columns"]] , "ID, TIME, DV, CMT, EVID, AMT, RATE, CL, V1, K, K12, K21, SCRT, AGE, SEX, BW, CLCR, IPRED, PRED, WRES, IRES, IWRES", msg = "check 1 (columns)")
  checkEquals(vec1[["NoHeader"]], FALSE, msg = "check 1 (noheader)")
	checkTrue(vec1[["append"]], msg = "check 1 (append)")

	vec2 <- RNMImport:::.importNmModTables(
    c("$TABLE ID TIME CL Q V1 V2 VSS MDV CYCL WT CMT IPRE IRES IWRS",
      "ONEHEADER NOPRINT",
      "FILE=TES8BA03.fit "))
  df2 <- data.frame(
    File="TES8BA03.fit",
    Columns="ID, TIME, CL, Q, V1, V2, VSS, MDV, CYCL, WT, CMT, IPRE, IRES, IWRS",
    NoHeader=FALSE, stringsAsFactors = FALSE)
	checkEquals( vec2$File    , df2$File, msg = "check 2 (file)")
  checkEquals( vec2$Columns , df2$Columns, msg = "check 3 (columns)")
  checkEquals( vec2$NoHeader, df2$NoHeader, msg = "check 2 (oneheader)")
	checkTrue(vec2[["append"]], msg = "check 2 (append)")
  
  
	vec3 <- RNMImport:::.importNmModTables(
    c("$TABLE ID TIME AMT IPRE DOSE FAST VMAX",
      "NOPRINT ONEHEADER FILE=TES11ST1.fit ",
      "$TABLE ID AMT VM1 VM2 KDKA CL V2 ALAG1 KD BMAX KDIS F1 TC DOSE FAST",
      "ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 ETA7 ETA8 ETA9 ETA10 ETA11",
      "NOPRINT NOAPPEND FIRSTONLY FILE=TES11ST1par.fit "))
	checkEquals( vec3$File    , c("TES11ST1.fit", "TES11ST1par.fit") , msg = "check 3 (file)")
  checkEquals( vec3$Columns , c("ID, TIME, AMT, IPRE, DOSE, FAST, VMAX",
                                "ID, AMT, VM1, VM2, KDKA, CL, V2, ALAG1, KD, BMAX, KDIS, F1, TC, DOSE, FAST, ETA1, ETA2, ETA3, ETA4, ETA5, ETA6, ETA7, ETA8, ETA9, ETA10, ETA11"), msg = "check 3 (columns)")
  checkEquals( vec3$NoHeader, rep(FALSE,2), msg = "check 3 (oneheader)")
  checkEquals( vec3$firstOnly, c(FALSE, TRUE), msg = "check3 (first only")
	checkEquals(vec3[["append"]], c(TRUE, FALSE) , " check 3 (append)")
  
	vec4 <- RNMImport:::.importNmModTables(
    c("$TABLE ID TIME DV IPRED PRED WRES IWRES IRES MDV ATIM OCC GRP ONEHEADER NOPRINT",
      "FILE=TES3ST03a.fit ",
	    "$TABLE ID CL K30 V3 K34 K43  K12 K23 OCC GRP F1 ALAG1 ONEHEADER",
      "NOPRINT FILE=TES3ST03p.fit ",
	    "$TABLE NOPRINT ONEHEADER ID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 ETA7 ETA8",
      "FILE=TES3ST03e.fit "))
	checkEquals( vec4$File    , c("TES3ST03a.fit", "TES3ST03p.fit", "TES3ST03e.fit") , msg = "check 4  (file)")
  checkEquals( vec4$Columns , c("ID, TIME, DV, IPRED, PRED, WRES, IWRES, IRES, MDV, ATIM, OCC, GRP",
                                "ID, CL, K30, V3, K34, K43, K12, K23, OCC, GRP, F1, ALAG1",
                                "ID, ETA1, ETA2, ETA3, ETA4, ETA5, ETA6, ETA7, ETA8"), msg = "check 4 (columns)")
  checkEquals( vec4$NoHeader, rep(FALSE,3), msg = "check 4 (oneheader)")

	checkEquals(vec4$append, rep(TRUE, 3) , msg = " check 4 (append)")

}



