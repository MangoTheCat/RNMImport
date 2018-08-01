\name{MiscExtractors}
\alias{getFileinfo}
\alias{getControlStatements}
\alias{getControltext}
\alias{getReporttext}
\alias{getNmVersion}
\alias{getSimInfo}

\title{Various miscellaneous "getter"/extractor functions}

\description{\code{getFileinfo} gets information about the report and control files, 
	\code{getControlStatements} extracts parsed control file statements as a list, \code{getControltext} extracts raw control file text,
	 \code{getReporttext} extracts raw report file text, \code{getNmVersion} returns information about the NONMEM version used to create a run or problem.
	 \code{getSimInfo} retrieves simulation information (number of simulations, seeds) from a simulation problem.}
 
 \usage{
 		getFileinfo(run)
 		getControlStatements(obj, ...)
 		getControltext(run)
 		getReporttext(run)
 		getNmVersion(obj)
 		getSimInfo(obj, problemNum = 1, addRawInfo = TRUE)
	}
 
 \arguments{
 	\item{run}{An object of class NMRun}
 	\item{obj}{An object of class NMRun, or one that extends NMProblem (for getSimInfo, should be a problem with simulation step)}
 	\item{problemNum}{[N,1] Number of the problem if obj is a run}
 	\item{addRawInfo}{[L,1] Should the raw contents of the $SIM statement be added to the returned information?}
 	\item{...}{If used on an NMRun object, should be \code{problemNum}} 
 	}
 	
 \value{\code{getFileInfo}: A data.frame holding information about the source file of the NONMEM run, and the 
 report. \code{getControlStatements}: The parsed control file statements corresponding to a particular 
 NONMEM problem. \code{getControltext}, \code{getReporttext}: The raw text of a control file that generated a run,
 or the report file that was produced respectively.  \code{getNmVersion}: Named character vector with NONMEM major and minor versions.
 \code{getSimInfo} A numeric vector with 3 entries : numSimulations, seed1, and seed2.  These are self-explanatory.
 If addRawInfo is TRUE, the returned result will have an attribute named "rawStatement" that will have the text
 of the control stream $SIM field. }
 
 \author{Mango Solutions <support@mango-solutions.com>}
 
 \keyword{methods}
 \keyword{utilities}

