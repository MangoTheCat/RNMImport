\name{getEstimateCov}
\alias{getEstimateCov}

\title{Returns variance-covariance matrix, if available,  from a NONMEM object }

\description{Retrieved the variance-covariance matrix of the estimators and optionally
 the correlation and inverse correlation matrices of the selected NONMEM run}
 
 \usage{getEstimateCov(obj, corMatrix = FALSE, invCorMatrix = FALSE, pdMatrix = FALSE, ...) }
 
 \arguments{
 	\item{obj}{An object of class NMRun or NMBasicModel}
	\item{corMatrix}{TRUE or FALSE.  If TRUE, will return the correlation matrix together with covariance matrix, in a list.} 	
 	\item{invCorMatrix}{TRUE or FALSE, not implemented yet}
 	\item{pdMatrix}{TRUE or FALSE, not implemented yet}
 	\item{...}{Additional arguments to other methods, e.g. problemNum if obj is of class NMRun or method if the referenced problem is a NONMEM 7 run}
 	}
 	
 \value{A matrix if just the covariance matrix is required,  
 a list of matrices otherwise}
 
 \author{Mango Solutions}
 \keyword{methods}
 \keyword{utilities}