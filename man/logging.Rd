\name{logMessage}
\alias{logMessage}
\alias{setLogConnection}
\alias{setLogFile}
\alias{closeLogConnection}
\alias{logConnection}
\alias{availableLogs}

\title{Logging utilities}

\description{These are a set of utilities for logging messages
to various logs. Many of the routines in this package use them to output additional 
diagnostic information which may help to track down problems.}

\usage{
	logMessage(text, logName)
	setLogConnection(logName, conn)
	setLogFile(logName, fileName)
	closeLogConnection(logName, allowFail=TRUE)
	logConnection(logName)
	availableLogs()
}

\arguments{
	\item{text}{Character vector of strings to write to log}
	\item{logName}{String name of a log}
	\item{conn}{A connection object}
	\item{fileName}{Name of the file that the log should point to}
	\item{allowFail}{ Logical flag.  If TRUE, will allow a closure of the log connection to fail, 
		 but will still disassociate the log with that connection. This is meant to be used with
		  uncloseable connections such as stdout. }
	}

\details{The individual functions to the following: \code{logMessage} logs a message to a 
	given log. \code{setLogConnection} points a log to a given connection, and \code{setLogFile} is a wrapper
	that points to a file connection.	\code{closeLogConnection} tries to close a log and its connection.
	\code{logConnection} returns a log associated to a particular connection, and \code{availableLogs}
	lists the set of available logs.
	}
	
\author{Mango Solutions <support@mango-solutions.com>}
\keyword{debugging}
