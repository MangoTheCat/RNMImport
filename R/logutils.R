
#' Writes a character vector to some given log, if that log is open
#' @param text Character vector to output
#' @param logName Name of the log.  Should be one of the available logs
#' @title Message logging
#' @return None
#' @author Mango Solutions
#' @keywords IO, debugging, utilities

logMessage <- function(text, logName)
{
	if(!is.na(logConnection(logName)))
		cat(text, file = .RNMImportEnv$logConnections[[logName]])
}

#' Sets a log's output connection to a given connection
#' @param logName Name of the log to modify 
#' @param conn Connection
#' @title Set log connection
#' @return Nothing
#' @author Mango Solutions
#' @keywords IO, debugging, utilities

setLogConnection <- function(logName, conn)
{
	if(!(logName %in% availableLogs()))
		RNMImportStop(paste(logName, "is not an existing log"))
	assertClass(conn, "connection")
	.RNMImportEnv$logConnections[[logName]] <- conn
}

#' Closes a connection where a log's output goes
#' @param logName The name of the log to close
#' @param allowFail Logical flag.  If TRUE, will allow a closure of the log connection to fail, 
#' but will still disassociate the log with that connection. This is meant to be used with unclosable connections such as stdout
#' @title Close log connection
#' @return Nothing
#' @author Mango Solutions
#' @keywords IO

closeLogConnection <- function(logName, allowFail = TRUE)
{
	
	if(!(logName %in% availableLogs()))
		RNMImportStop(paste(logName, "is not an existing log"), call = match.call())
	# Some logs, such as stdout, cannot be closed, so we must use a TRY here
	x <- try(close(.RNMImportEnv$logConnections[[logName]]), silent = TRUE )
	if(inherits(x, "try-error"))
	{
		if(allowFail)
			RNMImportWarning(paste("Unable to close the connection " %pst% logName, ", but proceeding anyway \n"), match.call())
		else
			RNMImportStop(paste("Unable to close the connection " %pst% logName, "\n"), match.call())
	}
	.RNMImportEnv$logConnections[[logName]] <- NA	
}

#' Points a given log to a file connection
#' @param logName Name of the log
#' @param fileName Name of the file (need not exist)
#' @title Set log connection to a file
#' @return None
#' @author Mango Solutions
#' @keywords IO

setLogFile <- function(logName, fileName)
{
	conn <- file(description = fileName, open = "w")
	setLogConnection(logName, conn)
}

#' Retrieves the connection associated with a particular log
#' @param logName The name of the log
#' @title Get log's connection
#' @return Connection associated to a log
#' @author Mango Solutions
#' @keywords utilities

logConnection <- function(logName)
{
	if(!(logName %in% availableLogs()))
		RNMImportStop(paste(logName, "is not an existing log"))
	.RNMImportEnv$logConnections[[logName]]
}

#' Retrieves the names of all available logs
#' @title Get all available logs
#' @return Character vector of all available logs
#' @author Mango Solutions
#' @keywords utilities, programming

availableLogs <- function()
{
	names(.RNMImportEnv$logConnections)
}

# internal utility function to extract full log config.  Meant to be used with unit tests to reload
# logs after running tests.

.getLogConfig <- function()
{
	.RNMImportEnv$logConnections
}
