

#' Creates a 3d-array from a list of matrices of the same dimension
#' @param matrixList A list of matrices of the same dimension
#' @param matrixNames The names of the matrices, to be used as the 3rd dimension names of the array
#' @title Convert list of matrices to 3d array
#' @return An 3d array whose submatrices come from "matrixList"
#' @author Mango Solutions

arrayFromMatrixList <- function(matrixList, matrixNames = names(matrixList))
{
	# check for more than one matrix, and that all elements of matrixList are of class matrix
	RNMImportStopifnot( length(matrixList) > 0 )
	RNMImportStopifnot( all( sapply(matrixList, inherits, "matrix") ) )
	
	# if no matrix names were given, construct some
	
	if(is.null(matrixNames))
		matrixNames <- paste("matrix", sep = "", seq_along(matrixList))
	
	# allocate the array to return
	
	result <- array( NA, dim = c(dim(matrixList[[1]]), length(matrixList)))
	
	for(i in seq_along(matrixList))
		result[,,i] <- matrixList[[i]]
	dimnames(result) <- c(dimnames(matrixList[[1]]), list(matrixNames)) 
	result
	
}
