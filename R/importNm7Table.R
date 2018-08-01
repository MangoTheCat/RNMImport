
#' Imports one of the new files produced by NONMEM 7: .phi, .cor, .cov, etc. into a  list of data.frames.
#' @param file Name of the file
#' @param path Path to the file (can be a NONMEM path in round brackets)
#' @param type Type of the file: One of "cov", "ext", or "phi".  "cov" should be used for .cor, .cov or .coi files.
#' @param tableTitles Single logical flag. Are the table titles present in the file?  Should be FALSE, if, for example, one generates an EXT
#' file using the NOTITLE=1 option.  Currently this option ONLY works when type = "ext"
#' @title Import new NONMEM 7 table files
#' @return a list of data.frames holding the content of the individual sub-tables within each file.  Each
#' table will have as an attribute "method", holding the name of the method associated to it, if it was available (e.g. not if
#' tableTitles = FALSE)
#' @author Mango Solutions
#' @note The "cov" type covers .cor, .cov and .coi files.
#' @keywords IO
#' @examples
#' \dontrun{ 
#'     phiTables <- importNm7Tables(type = "phi", "TestData1.phi", path = "testing/testdata/TestDataNM7")
#'     print(phiTables[[1]])
#'     print(attr(phiTables[[1]], "method"))
#' }
#' @export

importNm7Tables <- function(file, path = ".", type = c("cov", "ext", "phi"), tableTitles = TRUE)
{
	type <- match.arg(type)
	filePath <- processPath(path)
	
	# tableTitles will not apply to other function types for now
	
	if(type != "ext") tableTitles <- TRUE
	
	# regular expression that indicates where the tables start
	
	TABLEDELIMETER <- "^[[:space:]]*TABLE NO.[[:space:]]*[0-9]+"
	
	# try to read the file in, capturing the exception if necessary
	
	fileContents <- try( readLines(file.path(filePath,file)))
	if(inherits(fileContents, "try-error")) RNMImportStop( "Unable to load file" )
	
	# determine where tables begin and end
	if(tableTitles)
		tableBoundaries <- grep(fileContents, pattern = TABLEDELIMETER )
	else
		tableBoundaries <- grep(fileContents, pattern = "ITERATION")
	if(length(tableBoundaries) == 0) RNMImportStop( "No table delimeters found in the file : not a valid covariance file?" )
	
	else if(length(tableBoundaries) == 1) tableList <- list(fileContents[tableBoundaries:length(fileContents)])
	else tableList <- splitVector(fileContents, tableBoundaries, includeEnd = TRUE)
	if(tableTitles)
	{
		tableHeadings <- sapply(tableList, function(x) x[1])
		tableBodies <- lapply(tableList, function(x) tail(x, -1))
	}
	else
	{
		tableHeadings <- NULL
		tableBodies <- tableList
	}
	
	tableMethodNames <- vector(mode = "character", length = length(tableHeadings))
	
	for(i in seq_along(tableHeadings))
	{
		tableMethodNames[i] <- gsub(strsplit(tableHeadings[i], split = ":")[[1]][2], 
				pattern = "^[[:space:]]+", replacement = "" )
	}
	# all file types are handled identically except for "phi" files, where the first column is not
	# treated as a set of row names
	if(type %in% c("phi", "ext")) row.names <- NULL else row.names <- 1
	
	# covariance matrices should be returned as matrices, not data.frames
	if(type == "cov") coerceToMatrix <- TRUE else coerceToMatrix <- FALSE
	# read actual contents into individual tables
	tableContents <- lapply( seq_along(tableBodies), 
			function(i) {
				tableTextConn <- textConnection(tableBodies[[i]])
				y <- read.table(tableTextConn, header = TRUE, row.names = row.names, stringsAsFactors = FALSE)
				close(tableTextConn)
				if(coerceToMatrix) y <- as.matrix(y)
				# method names will be missing if there are no titles
				if(length(tableMethodNames) > 0)
					attr(y, "method") <- tableMethodNames[[i]]
				y
			})
	
	tableContents
}
