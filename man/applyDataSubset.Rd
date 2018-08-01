\name{DataSubsetting}
\alias{applyDataSubset}
\alias{dataSubset}
\alias{dataSubset<-}
\alias{augmentDataSubset<-}

\title{Modify and apply data subsets}
\description{
	 In analyzing (and especially graphing) NONMEM data sets, it is often convenient to work with 
  subsets of data that exclude certain rows of data, such as dosing data.  These four functions can be
  used to modify and retrieve associated data subsets with particular NONMEM data objects.
}
\usage{
	applyDataSubset(obj, sub = NULL, verboseWarnings = FALSE)
	dataSubset(x)
	dataSubset(x) <- value
	augmentDataSubset(x) <- value
	
}

\arguments{
  \item{x}{Object (NMProblem or data.frame) whose subset should be modified or retrieved.}
  \item{obj}{ Object of class data.frame to which a subset will be applied}
  \item{value}{Character vector of subset expressions}
  \item{sub}{ Character vector of subset expressions }
  \item{verboseWarnings}{ Logical flag.  If TRUE, will emit a warning whenever a subset statement fails }
}

\value{
	\code{applyDataSubset} will return the data.frame obj with the (valid) subsetting expressions applied.  
	\code{dataSubset} the data subset associated with an object.
}

\examples{
	\dontrun{
		x <- importNm("TestData1.ctl", "TestData1.lst", path  = "testing/testdata/TestRun")
		prob <- getProblem(x)
		print(dataSubset(prob))
		z <- nmData(prob, subset = c(dataSubset(prob), "ID < 20"))
		print(summary(z))
	
	}
}
\keyword{ manip }

