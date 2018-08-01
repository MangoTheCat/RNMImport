# $LastChangedDate: 2014-07-22 09:21:30 +0100 (Tue, 22 Jul 2014) $
# $LastChangedBy: jli@MANGO.LOCAL $
# $Rev: 115297 $
# 
# Author: fgochez
###############################################################################

# assumes global var unitTestPath points to the path where unit tests are located, e.g.
# RNMImport/inst/unittests

test.importNm7Tables <- function()
{
	unitTestPath <- get("TestPath", envir = .RNMImportTestEnv)
	testDataPath <- file.path(unitTestPath, "testdata/TestDataNM7")
	
	# correlation matrix
	corFile <- importNm7Tables("TestData1.cor", path = testDataPath, type = "cov")
	covFile <- importNm7Tables("TestData1.cov", path = testDataPath, type = "cov")
	
	# parameter iteration file
	extFile <- importNm7Tables("TestData1.ext", path = testDataPath, type = "ext")
	
	#phi file
	phiFile <- importNm7Tables("TestData1.phi", path = testDataPath, type = "phi")
	
	# check correlation matrix
	# only one element should be found
	checkEquals(length(corFile), 1, " |only one method has a correlation matrix available")
	corMat <- corFile[[1]]
	
	# correct method name
	checkEquals( attr(corMat, "method"), "Iterative Two Stage", msg = " |method name correct" )
	
	# check for correct dimension
	checkEquals(dim(corMat), c(10, 10), msg = " |correct dimensions of correlation matrix" )
	
	# correct row and column names
	checkEquals(dimnames(corMat), list(c("THETA1", "THETA2", "THETA3", "SIGMA(1,1)", "OMEGA(1,1)", 
							"OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)"
					), c("THETA1", "THETA2", "THETA3", "SIGMA.1.1.", "OMEGA.1.1.", 
							"OMEGA.2.1.", "OMEGA.2.2.", "OMEGA.3.1.", "OMEGA.3.2.", "OMEGA.3.3."
					)), msg = " |dim names correct")
	
	checkEquals(class(corMat), "matrix", msg = " |correct class for correlation matrix")
	
	checkTrue( all(-1 <= corMat & corMat <= 1), msg = " |all correlations between -1 and 1")
	checkTrue(all(diag(corMat) %in% c(0,1)), msg = " |diagonal of correlations either 1 or 0" )
	
	## covariance matrix file
	
	checkEquals(length(covFile), 1, msg = " |correct number of covariance matrices")
	covMat <- covFile[[1]]
	checkEquals( attr(covMat, "method"), "Iterative Two Stage", msg = " |method name correct" )
	
	# check for correct dimension
	checkEquals(dim(covMat), c(10, 10), msg = " |correct dimensions of covariance matrix" )
	
	# correct row and column names
	checkEquals(dimnames(covMat), list(c("THETA1", "THETA2", "THETA3", "SIGMA(1,1)", "OMEGA(1,1)", 
							"OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)"
					), c("THETA1", "THETA2", "THETA3", "SIGMA.1.1.", "OMEGA.1.1.", 
							"OMEGA.2.1.", "OMEGA.2.2.", "OMEGA.3.1.", "OMEGA.3.2.", "OMEGA.3.3."
					)), msg = " |dim names correct")
	
	checkEquals(class(covMat), "matrix", msg = " |correct class for correlation matrix")
	
	# checkTrue( all(-1 <= covMat & covMat <= 1), msg = " |all correlations between -1 and 1")
	checkTrue(all(diag(covMat >= 0)), msg = " |diagonal of covariances greater than or equal 0" )
	
	# now check that the correlation matrix (minus columns with all zeros) is equal to the covariance matrix over 
	#product of variances
	# get zero columns / rows
	zeroEntries <- apply(corMat, 1, function(x) all(x == 0))
	
	corMat2 <- corMat[ !zeroEntries, !zeroEntries ]
	covMat2 <- covMat[ !zeroEntries, !zeroEntries ]
	
	checkEquals(covMat2 / outer(sqrt(diag(covMat2)), sqrt(diag(covMat2))), corMat2, tol = 0.00001, 
			msg = " |correlation matrix is covariance with variance produces divided out")
	
	
	#### check ext file
	checkEquals(length(extFile), 2, msg = " |2 parameter iteration entries")
	## first element
	ext1 <- extFile[[1]]
	checkEquals(attr(ext1, "method"),"Iterative Two Stage", msg = " |correct method")
	checkEquals(dim(ext1), c(23, 12), msg = " |correct dimensions (ext1)" )
	# check 3 rows are correct
	checkEquals(as.matrix(ext1[c(1, 10, 21),], ), structure(c(0, 45, 100, 18.7, 18.0166, 20.0324, 87.3, 86.7684, 
							77.252, 2.13, 1.24267, 1.26627, 0.0231, 0.0258688, 0.0258948, 
							0.128, 0.145414, 0.156811, 0, 0, 0, 0.142, 0.169365, 0.162315, 
							0, 0, 0, 0, 0, 0, 1.82, 0.76139, 0.737196, 3353.01717248576, 
							3335.78144602791, 3335.24999170082), .Dim = c(3L, 12L), .Dimnames = list(
							c("1", "10", "21"), c("ITERATION", "THETA1", "THETA2", "THETA3", 
									"SIGMA.1.1.", "OMEGA.1.1.", "OMEGA.2.1.", "OMEGA.2.2.", "OMEGA.3.1.", 
									"OMEGA.3.2.", "OMEGA.3.3.", "OBJ"))),
			msg = " | 3 arbitary ext (1) rows correct")
	
	ext2 <- extFile[[2]]
	checkEquals(dim(ext2), c(19, 12), msg = " |correct dimensions (ext2)")
	checkEquals(attr(ext2, "method"),"Stochastic Approximation Expectation-Maximization", msg = " |correct method")
	
	checkEquals(as.matrix(ext2[c(1, 10, 18),] ), structure(c(-2000, -1100, 500, 20.0324, 18.8211, 19.1414, 77.252, 
							81.0599, 76.7051, 1.26627, 1.77679, 1.68038, 0.0258948, 0.0273221, 
							0.0265968, 0.156811, 0.143025, 0.144997, 0, 0, 0, 0.162315, 0.154808, 
							0.149245, 0, 0, 0, 0, 0, 0, 0.737196, 1.40331, 1.41989, 2156.55055725555, 
							2409.44674555807, 2339.09337466099), .Dim = c(3L, 12L), .Dimnames = list(
							c("1", "10", "18"), c("ITERATION", "THETA1", "THETA2", "THETA3", 
									"SIGMA.1.1.", "OMEGA.1.1.", "OMEGA.2.1.", "OMEGA.2.2.", "OMEGA.3.1.", 
									"OMEGA.3.2.", "OMEGA.3.3.", "SAEMOBJ"))), msg = " |3 arbitary ext (2) rows correct")
	### phi file
	
	phi1 <- phiFile[[1]]
	phi2 <- phiFile[[2]]
	checkEquals(as.matrix(phi1[ c(1, 50, 99), ]), structure(c(1, 50, 99, 1, 34, 65, -0.0892195, 0.208854, -0.00669994, 
							-0.0571484, 0.145017, 0.147702, 0.508486, -0.947682, -0.314036, 
							0.00365536, 0.00367456, 0.00275224, 0.00422405, -0.00163814, 
							0.00346855, 0.0110538, 0.0896997, 0.0167218, -0.0105421, -0.0101178, 
							-0.00194681, 0.00969209, 0.116645, 0.023809, 0.225226, 0.189915, 
							0.0890838, 50.3294403587607, 2.11248390374376, 60.1407714521398
					), .Dim = c(3L, 12L), .Dimnames = list(c("1", "50", "99"), c("SUBJECT_NO", 
									"ID", "PHI.1.", "PHI.2.", "PHI.3.", "PHC.1.1.", "PHC.2.1.", "PHC.2.2.", 
									"PHC.3.1.", "PHC.3.2.", "PHC.3.3.", "OBJ"))) )
	
	checkEquals(dim(phi1), c(99, 12))
	meths <- c(attr(phi1, "method"), attr(phi2, "method"))
	
	checkEquals(meths, c("Iterative Two Stage", "Stochastic Approximation Expectation-Maximization" ), msg = " |correct method names extracted")
	
	checkEquals( as.matrix(phi2[ c(1, 50, 99), ]), 
			structure(c(1, 50, 99, 1, 34, 65, -0.0697335, 0.205404, 0.0117088, 
									-0.0521237, 0.152746, 0.10186, 0.748874, -0.838038, -0.395001, 
									0.00363284, 0.00839403, 0.00445682, 0.00436218, -0.00616701, 
									0.00142263, 0.0116209, 0.0784169, 0.0421257, -0.0133319, -0.0617564, 
									-0.0239624, 0.0124552, 0.155796, 0.0751236, 0.54612, 0.837211, 
									0.456284, 39.7884222477242, -6.64586539926693, 49.1471758052572
							), .Dim = c(3L, 12L), .Dimnames = list(c("1", "50", "99"), c("SUBJECT_NO", 
											"ID", "PHI.1.", "PHI.2.", "PHI.3.", "PHC.1.1.", "PHC.2.1.", "PHC.2.2.", 
											"PHC.3.1.", "PHC.3.2.", "PHC.3.3.", "SAEMOBJ"))))	
			
	# regression test for issue 2393
	# check that default path works
	currentPath <- getwd()
	setwd(testDataPath)
	x <- importNm7Tables("TestData1.ext", type = "ext")
	checkEquals(x, extFile, msg = " |EXT file succesfully loaded with default path")
	
	setwd(currentPath)
	
	# issue 2312 : check NOTITLES works correctly
	extNoTitle1Test <- importNm7Tables("TestData1_2.EXT", path = testDataPath, type = "ext", tableTitles = FALSE) 
	extNoTitle2Test <- importNm7Tables("extfile2_2.EXT", path = testDataPath, type = "ext", tableTitles = TRUE)
	
	checkEquals(attr(extNoTitle2Test[[1]], "method"), "Stochastic Approximation Expectation-Maximization", msg = " |table title present")
	checkTrue(is.null(attr(extNoTitle1Test[[1]], "method")), msg = " |table title not present")
	
	checkEquals(extNoTitle1Test[[1]], extFile[[1]], check.attributes = FALSE )
	checkEquals(extNoTitle2Test[[1]], extFile[[2]], check.attributes = FALSE )
}