\name{importNm}
\alias{importNm}
\title{Import a NONMEM run}
\usage{importNm(conFile, reportFile, path, dropInputColumns=TRUE, textReport=FALSE)
}
\description{Imports a NONMEM run based on a control file and an output list file.  This function assumes that 
all of the input and output data tables are in the same directory as the control file, though the input and output
tables are allowed to be missing. An object of class \code{NMRun} is returned#'}
\value{An NMRun object that holds the information of the NONMEM run. If input data tables cannot be imported, 
a warninjg will ne emitted and the input data will be ignored.  The same is true if any output table files 
cannot be found.}
\note{The behaviour of this function is fairly complex, due to the intricacies of NONMEM itself. Below we list key
behaviours relating to various categories.
\itemize{
\item{Input data:}{Input data is specified by the control file $INPUT and $DATA statements. Dropped 
columns will be managed according to the value of the dropInputColumns parameter. Aliased columns will be duplicated, once with their alias, 
and once with their alternative name (e.g. the column SEX=GEN would appear as a column SEX and once as GEN). If there are more columns in the input data than are 
referenced in the $DATA statement they will be attached with the names ExtraCol[x].  Ignored data (as specified by the IGNORE= directive) will not
be available in the returned object. } 
\item{Iterations (NONMEM 7 only):}{NONMEM 7-style iteration files (".ext") will be imported as the iterations for a NONMEM 7 run (if available).
The name of the iterations file will be automatically deduced from the control file.  They will be retrievable with the \code{getIterations}
function. } 
}}
\author{Mango Solutions}
\keyword{IO}
\arguments{\item{conFile}{Single string with control file name}
\item{reportFile}{The name of the report file.  In the future this parameter will be optional, 
in which case importNm will attempt' to deduce the file name on its own based on the name of the conFile
and the list file extensions stored in the metadata.}
\item{path}{The path where the files are located.  Can be the name of a path stored with setNmPath if it
is surrounded by round brackets}
\item{dropInputColumns}{Logical flag.  If TRUE, those columns of the input data flagged for dropping in the 
$INPUT statement will not be imported with the run}
\item{textReport}{Logical flag.  If TRUE, various standard text outputs will be logged to stdReport}
}

