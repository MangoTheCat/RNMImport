
#' @title Import model tables
#' @description Imports the output data used in an individual NONMEM model based on its $TABLE statements 
#' 
#' @param tableStatement tableStatement [char matrix] - A control file table statement, as parsed by .importNmModTables 
#' @param allowFirstOnly [L,1] - Allow the use of FIRSTONLY statements in the $TABLE statement? 
#' @param dvLog currently unused
#' @param trim If TRUE, will _not_ add additional variables such as absWRES to the returned data
#' @param returnFormat How to return data. If "DFList", a list of data.frames, with one for each table file produced.
#' Otherwise, attempts to consolidate all of the (unique) output table variables into a single data.frame, but will return
#' a list of some of the tables had a FIRSTONLY statement, and others did not.
#' @param path Path to the table files.  Can be a path name 
#' @return a list or a data.frame.  
#' A data.frame of all unique output table columns (from all table files)
#' is returned if returnFormat = "singleDF", 
#' UNLESS there are both FIRSTONLY tables and non-FIRSTONLY tables, in which
#' case a list of 2 components is returned. 
#' @author Mango Solutions
#' @importFrom methods is
#' @export

importModelOutputTables <- function( tableStatement, allowFirstOnly = TRUE, 
        dvLog = FALSE, trim = FALSE, returnFormat = c("singleDF", "DFlist"),
        path = NULL) {	

    tableStatement = normalize.table.statements(tableStatement)
	NUMEXPECTEDCOLUMNS <- 5

	FILEFIELD <- "File"
	FIRSTONLYFIELD <- "firstOnly"
	
    APPENDEDCOLUMNS <- c("DV", "PRED", "RES", "WRES") 
    ignore.by.nonmem.table <- c('PRED','RES','WRES')
	
	logMessage(logName = "detailedReport", "Importing output tables\n")
	returnFormat <- match.arg(returnFormat)
	
	numStatements <- nrow(tableStatement)
	tableList <- vector(mode = "list", length = numStatements)
	
    # going through tables
    normalize.word.table = list(
                            list(from='IPRE',to='IPRED'),
                            list(from='IWRE',to='IWRES')
                        )
    normalize.colname <- function(v){
        for(i in normalize.word.table) {
            v = replace(v, which(v == i$from), i$to)
        }
        v
    }

	for(i in 1:numStatements) {
		currentTable <- try(readNmData(file = .getFile(tableStatement[i, FILEFIELD], path = path)), 
				silent = TRUE)
		
		# try to read table file, emitting a warning if it fails and continuing to next
		if(inherits(currentTable, "try-error"))
		{
			msg <- paste("Unable to read table file", tableStatement[i, FILEFIELD], "due to error", currentTable$message, "\n") 
			RNMImportWarning(msg, call = match.call())
			#tableList[[i]] <- NA
			next
		} 


		# force to numeric
		currentTable <- .importDataNumeric(currentTable, missToZero = FALSE)
		
        # compute the correct pre.colNames, colNames ; convert maybe factors into characters
        colNames <- CSLtoVector(as.character(tableStatement[i,"Columns"]))
        pre.colNames <- colNames
        # pre.colNames : the colNames read from tableStatement and modified according to append attribute
        #     colNames : the colNames read from tableStatement but NOT modified according to append attribute
		if(tableStatement[i, "append"]) {
            # if APPEND is TRUE, then we need to extract the column names from "appendedColumns", 
            #  and then append them back to the end.
            # this is necessary because if APPEND is used (which it is by default), 
            # NONMEM appears to ignore the presence of PRED, RES and WRES in the table statement, 
            # and simply adds them to the end of the table on its own 
            # regardless of what order they appear in the TABLE statement
            # However, "DV" won't be ignored except it's happened to be the last one in the statement
            # "DV" appear in other place will remain
            if (pre.colNames[length(pre.colNames)]=='DV') {
                pre.colNames <- pre.colNames[-length(pre.colNames)]
            }
            pre.colNames <- pre.colNames[is.na(match(pre.colNames, ignore.by.nonmem.table))]
            pre.colNames <- c(pre.colNames, APPENDEDCOLUMNS)
		}
        # we amend the column Name to the computed one
        if (length(pre.colNames) == NCOL(currentTable)) {
            # For cwres tables, the tableStatement might be incorrectly 
            # (see importNm function , the APPEND is set to TRUE directly) setting the 
            # APPEND attribute, however, there is not columns appended
            # we should insure to change colnames only the NCOL are coincided
            colnames(currentTable) <- pre.colNames
        } else if (length(colNames) == NCOL(currentTable)) {
            colnames(currentTable) <- colNames
        } else  {
            RNMImportWarning(sprintf('The computed column names (%d columns)
            [%s] 
            is not match to actual table names (%d columns)
            [%s]
            .', length(pre.colNames) , paste(pre.colNames,collapse=','), 
            NCOL(currentTable), paste(colnames(currentTable),collapse=',')) )
        }
        # or else the tableStatements are incorrect, the colNames are not changed
        # then, normalize those names
        colnames(currentTable) <- normalize.colname(colnames(currentTable))
        # Now handle FIRSTONLY statement if it is present.  We take unique values of the ID by default        
        if(isTRUE(allowFirstOnly)) {
            if (isTRUE(tableStatement[i, FIRSTONLYFIELD])) {
                logMessage("Firstonly flag found, subsetting rows", "detailedReport")
                attr(currentTable, FIRSTONLYFIELD) <- TRUE
            } else {
                attr(currentTable, FIRSTONLYFIELD) <- FALSE
            }
        } else {
            if (isTRUE(tableStatement[i, FIRSTONLYFIELD])) {
                RNMImportStop("FIRSTONLY table detected, yet allowFirstOnly is set to FALSE", match.call() )
            }
        }
        tableList[[i]] <- currentTable
    }
    # clear up temporary files
    if (!is.null(need.remove.files<-attr(tableStatement,'need.remove')) && length(need.remove.files)>0) {
        if(is(try(file.remove(need.remove.files),silent=T),'try-error')) {
            warning(sprintf('Not able to remove temporary files[%s], you can remove it yourself.', 
                paste(need.remove.files,collapse=',')))
        }
    }
    # cast output tables
    if( all(returnFormat == "DFList") ) return(tableList)

    # return a combined data frame
    # might be two, if some tables are first only table
    
    # util functions
    reorder.iter <- function(x) {
        # rotate the first 'iter' column to the end
        pivot = match('iter',colnames(x))
        if (!is.na(pivot)) {
            N = NCOL(x)
            if (pivot > 1 && pivot <N) {
                x <- x[,c(1:(pivot-1),(pivot+1):N,pivot)]
            }
        }
        x
    }

    cast.table <- function( ind, addAbsWRes=TRUE ) {
        nrows = sapply(tableList[ ind ], NROW)
        if ( all(nrows == nrows[1]) ) {
            out.tab = do.call(base::cbind, tableList[ ind ] )
        } else {
            ind.max = which.max(nrows)
            # when NROWs are not equal, cbind will give an error
            # we merge or fill NA's for the shorter ones
            out.tab = base::Reduce(f= function(x0,y0) {
                                        x0 = x0[,unique(colnames(x0))]
                                        y0 = y0[,unique(colnames(y0))]
                                        by0 = intersect(colnames(x0),colnames(y0))
                                        if (length(by0)>0) {
                                            if (NROW(x0) == NROW(y0)) {
                                                return( cbind(x0,y0) )
                                            } 
                                            foo = base::merge(x0,y0, by=by0,all.x=TRUE)
                                            if (NROW(foo) == NROW(x0)) {
                                                return(foo)
                                            }
                                            RNMImportWarning('
                                            Outer join happened, fall back to filling NAs then bind.
                                            However, this is usually because those 
                                            $TABLEs should not be merged together,
                                            it should be more appropriate to return a list,
                                            maybe you should choose returnFormat=DFList for this problem?
                                            ')
                                        } 
                                        # we don't want total outer join
                                        # fill NA
                                        y0[NROW(x0),] <- NA
                                        cbind(x0, y0)
                                    }, 
                x = tableList[ind[-ind.max]], 
                init=tableList[[ ind[ind.max] ]], 
                accumulate=FALSE)
        }
        out.tab = out.tab[, unique(colnames(out.tab)), drop=FALSE]
        if (addAbsWRes) out.tab = .deriveNmColumns(out.tab)
        #if (sim > 0 ) out.tab = reorder.iter(out.tab)
        out.tab
    }
				
    # get the index for two (possible) types of table
    null.ind = which(sapply(tableList, FUN = function(x) {
        is.null(x) || is.na(x) }))

    fol.ind = setdiff(which(  tableStatement[, FIRSTONLYFIELD] ), null.ind)
    nor.ind = setdiff(which( !tableStatement[, FIRSTONLYFIELD] ), null.ind)
    fol.exs <- nor.exs <- F

    if (length(fol.ind)>0) {
        fol.tab = cast.table( fol.ind, addAbsWRes=!trim )
        fol.exs = T
    }
    if (length(nor.ind)>0) {
        nor.tab = cast.table( nor.ind, addAbsWRes=!trim )
        nor.exs = T
    }
    if (nor.exs && fol.exs) {
        RNMImportWarning("Found tables of both FIRSTONLY and NON-FIRSTONLY type, returning a list")
        return(list("normal.tables" = nor.tab, "firstonly.tables" = fol.tab))
    }
    if (fol.exs) {
        RNMImportWarning("Found only tables of FIRSTONLY")
        return(fol.tab)
    }
    if (nor.exs) {
        return(nor.tab)
    }
    RNMImportWarning("None table is read successfully")
    NULL
}
