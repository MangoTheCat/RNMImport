

#' Renames IPRE to IPRED, IWRE to IWRES and adds absWRES to a NONMEM data table
#' @param df data.frame of NONMEM input or output table data
#' @title Rename certain NONMEM columns
#' @return data.frame
#' @author Mango Solutions


.deriveNmColumns <- function(df)
{
	dfNames <- names(df)

	dfNames <- replace(dfNames, which(dfNames == "IPRE"), "IPRED")
	dfNames <- replace(dfNames, which(dfNames == "IWRE"), "IWRES")
    names(df) <- dfNames
	if("WRES" %in% dfNames)
		df$absWRES <- abs(df$WRES)
	df
}
