
#' For a table statement, if user put multiple un-mergeble tables into one text output file
#' We split it into multiple table files as [origianlname]__1, [origianlname]__2 , e.t.c
#' to allow the importing function working correctly
#' @param tableStatement a tableStatement in the control stream, typically returned by importNmMod
#' @param force.change return the normalized tableStatement even the original file is not splited successfully
#' @return the normalized tableStatement which each table resides in its own file
normalize.table.statements = function(tableStatement, force.change=FALSE) {
    files = as.character(tableStatement[,'File'])
    N = NROW(tableStatement)
    if (N<2) return(tableStatement)
    tab = list()
    new.names = files
    for(i in 1:N){
        if (is.null(tab[[ files[i] ]] )) {
            tab[[ files[i] ]]  = 0
        } 
        tab[[ files[i] ]]  = tab[[ files[i] ]] + 1
        new.names[i] = sprintf('%s__%s',files[i] , tab[[ files[i] ]])
    }
    onames = names(tab)[ tab>1 ]
    if(length(onames)<1) return(tableStatement)
    result = logical(length(onames))
    no = 0
    for(i in onames) {
        result[ (no<-no+1) ] = split.file.into(i, tab[[ i ]])
    }
    if (!all(result)){
        warning(sprintf('The file(s) [%s] which contain multiple table statements are not splited successfully.', 
            paste(onames[ which(!result) ], collapse=',')))
    }
    if (!force.change) {
    # we do not change those unsuccessful split
        onames = onames[ result ]
        if (length(onames)<1) return(tableStatement)
    }
    new.file.ind = which(files %in% onames)
    tableStatement$File[ new.file.ind ] = new.names[new.file.ind]
    attr(tableStatement, 'need.remove') = new.names[new.file.ind]
    tableStatement
}

#' split a multiple table text file into multiple files which contain only one for each
#' @param fn the filename of multiple table text file
#' @param num how many tables need to be splitted into
#' @return logical value indicating if splitting is successfully
split.file.into = function(fn, num) {
    lines = try(readLines(fn), silent=TRUE)
    if (is(lines,'try-error')) return(FALSE)
    ind = grep('^TABLE NO.*\\d', lines)
    if (length(ind) !=num ) {
        warning(sprintf('The table file contain %s table(s), while requiring splitting into %s table(s)', length(ind), num))
        return(FALSE)
    }
    ind1 = c(ind, length(lines)+1)
    for(i in 1:length(ind)){
        writeLines(lines[ind1[i]:(ind1[i+1]-1)], con=sprintf('%s__%s',fn,i))
    }
    TRUE
}

