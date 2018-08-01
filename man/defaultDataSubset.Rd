\name{configsubset}
\alias{defaultDataSubset}
\alias{augmentDefaultDataSubset}
\alias{setDefaultDataSubset}

\title{ Configure the global data subset attached to NONMEM problems on loading }
\description{
  
  In analyzing (and especially graphing) NONMEM data sets, it is often convenient to work with 
  subsets of data that exclude certain rows of data, such as dosing data.  In this package, a "default"
  standard data subset is available for passing into various routines. defaultSubset will retrieve
  the currently set default subset.  setDefaultSubset will overwrite the existing subset, and also allows one to
  control whether the subset is attached to an object by default on loading.  augmentDefaultDataSubset allows one to
  append subset expressions to the current global default subset.
  
}
\usage{
	defaultDataSubset()
	setDefaultDataSubset(sub, applyOnLoad)
	augmentDefaultDataSubset(sub)
}

\arguments{
  \item{sub}{ A character vector of subsetting expressions, such as "MDV != 1", "EVID == 0" and so on. }
  \item{applyOnLoad}{ Logical.  Should the subset be attached to any NMProblem object upon loading? }
}

\details{
	When loading is done via \code{importNm}, the default subset may be associated to each constituent problem.
	If you wish to disable this, use \code{setDefaultSubset(sub, FALSE)}.
}
\value{
  \code{defaultDataSubset} will return a character vector of the current subsetting expressions.
}

\author{Mango Solutions}


\examples{
	print(defaultDataSubset())
	
}
\keyword{ utilities }
