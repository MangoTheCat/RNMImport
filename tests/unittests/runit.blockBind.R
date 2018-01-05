
## Test suite designed to test the blockBind function ##
test.blockBind <- function() {
	blockBind <- RNMImport:::blockBind
	# Build test matrices
	mat1 <- cbind(1)
	mat2 <- outer(1:2, 1:2)
	mat3 <- 1
	mat4 <- diag(1:5)
	
	# Create tests
	checkEquals( blockBind(list(mat1))                  , mat1)
	checkEquals( blockBind(list(mat2))                  , mat2)
	checkEquals( blockBind(list(mat3))                  , mat1)
	checkEquals( blockBind(list(mat4))                  , mat4)
	checkEquals( blockBind(list(mat1))                  , cbind(1))
	checkEquals( blockBind(list(mat3))                  , cbind(1))
	checkEquals( blockBind(list(mat1, mat3))            , diag(2))
	checkEquals( blockBind(list(mat1, mat2, mat3))      , matrix(c(1, 0, 0, 0, 0, 1, 2, 0, 0, 2, 4, 0, 0, 0, 0, 1), 4))
	checkEquals( blockBind(list(mat1, mat2, mat3, mat4)), matrix(c(1,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,2,4,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,5), 9))
	
	checkEquals( blockBind(list(mat2, mat2, mat3, mat4)), 
			blockBind(list(mat2, "SAME", mat3, mat4)), msg = "testing SAME" )
	
	checkEquals( blockBind(list(mat1, mat3, mat3, mat3)), 
			blockBind(list(mat1, mat3, "SAME", "SAME")), msg = "testing SAME" )
	
}

