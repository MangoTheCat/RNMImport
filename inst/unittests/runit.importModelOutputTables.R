# $LastChangedDate: 2014-07-22 09:21:30 +0100 (Tue, 22 Jul 2014) $
# $LastChangedBy: jli@MANGO.LOCAL $
# $Rev: 115297 $
# 
# Author: fgochez
###############################################################################


test.importModelOutputTables <- function()
{
	# we want to test that APPEND is handled correctly.  In this case, 
	
	tableStatement <-  "$TABLE  ONEHEADER ID STUD WEEK NITE DRUG DOSE A B C D
  	ETA(1) ETA(2) MDV WRES IWRES IPRED NOPRINT FILE=testtab.tab"
  	
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
  	tableInfo <- RNMImport:::.importNmModTables(tableStatement)
  	tableTest <- RNMImport:::importModelOutputTables(tableInfo, path = file.path(unitTestPath, "testdata"))
  	
  	checkEquals(as.matrix(tableTest), structure(c(11001, 11001, 11001, 11001, 11001, 3, 3, 3, 3, 3, 
		1, 1, 2, 2, 3, 1, 2, 1, 2, 1, 0, 0, 1, 1, 0, 0, 0, 10, 10, 25, 
		32.436, 32.436, 32.436, 32.436, 32.436, 1, 1, 1, 1, 1, 68.935, 
		68.935, 68.935, 68.935, 68.935, 0.29034, 0.29034, 0.29034, 0.29034, 
		0.29034, -0.68686, -0.68686, -0.68686, -0.68686, -0.68686, 0.91745, 
		0.91745, 0.91745, 0.91745, 0.91745, 0, 0, 0, 0, 0, -20.436, -6.9358, 
		-11.018, 6.4817, 15.197, 32.436, 32.436, 23.018, 23.018, 23.803, 
		12, 25.5, 12, 29.5, 39, 64.465, 64.465, 45.748, 45.748, 33.792, 
		-52.465, -38.965, -33.748, -16.248, 5.208, -0.84774, -0.48733, 
		-0.66465, -0.0062969, 0.51131, 0.84774, 0.48733, 0.66465, 0.0062969, 
		0.51131), .Dim = c(5L, 20L), .Dimnames = list(c("1", "2", "3", 
		"4", "5"), c("ID", "STUD", "WEEK", "NITE", "DRUG", "DOSE", "A", 
		"B", "C", "D", "ETA.1.", "ETA.2.", "MDV", "IWRES", "IPRED", "DV", 
		"PRED", "RES", "WRES", "absWRES"))), msg = " |table output captured correctly when APPEND is TRUE but an appended column is repeated")
  	
	# now check FIRSTONLY bug fix works
	tableStatement2 <- c("$TABLE ID TIME DV PRED RES WRES CPRED CWRES EPRED ERES EWRES NOAPPEND ONEHEADER FILE=tabletest2_1.TAB NOPRINT",   
	"$TABLE ID A B C D FIRSTONLY NOAPPEND NOPRINT FILE=tabletest2_2.tab")
	
	tableInfo2 <- RNMImport:::.importNmModTables(tableStatement2)
	tableTest2 <- RNMImport:::importModelOutputTables(tableInfo2, path = file.path(unitTestPath, "testdata"))
	
	checkEquals(names(tableTest2), c("normal.tables", "firstonly.tables"), msg = " |mix of table types found")
	
	normalTab <- tableTest2$normal.tables
	
	normTabExpected1 <- structure(c(1, 1, 0, 0.2, 0, 14.971, 19.963, 14.726, 0, 0.24453, 
					0, 0.047138, 19.958, 14.724, 0, 0.049868, 21.437, 14.714, 0, 
					0.25673, 0, 0.048926, 0, 0.047138), .Dim = c(2L, 12L), .Dimnames = list(
					c("1", "2"), c("ID", "TIME", "DV", "PRED", "RES", "WRES", 
							"CPRED", "CWRES", "EPRED", "ERES", "EWRES", "absWRES"))) 
	
	checkEquals(as.matrix(head( normalTab, n = 2 )), normTabExpected1, msg = " |normal table correct (check 1)")
	
	normTabExpected2 <- structure(c(2, 2, 40, 70, 0.067345, 0.013129, 0.0034156, 5.1172e-05, 
					0.063929, 0.013078, 4.9901, 143.99, -0.079153, -0.029415, 0.87142, 
					1.4946, 0.014428, 0.0015615, 0.052917, 0.011568, 1.1118, 1.4855, 
					4.9901, 143.99), .Dim = c(2L, 12L), .Dimnames = list(c("11", 
							"12"), c("ID", "TIME", "DV", "PRED", "RES", "WRES", "CPRED", 
							"CWRES", "EPRED", "ERES", "EWRES", "absWRES")))
	
	checkEquals(as.matrix(tail( normalTab, n = 2 )), normTabExpected2, msg = " |normal table correct (check 2)")
	
	firstSubtab <- tableTest2$firstonly.tables[, c("A", "B", "C", "D")]
	
	# check non-ID columns of firstonly table
	checkEquals(as.matrix(firstSubtab),  structure(c(5.6908, 3.2198, 5.1272, 8.6092, 2.2007, 2.0778, 12.621, 
							18.453), .Dim = c(2L, 4L), .Dimnames = list(c("1", "2"), c("A", 
									"B", "C", "D"))), msg = " | firstonly table (partially) correct" )
	### 
	
	
}
