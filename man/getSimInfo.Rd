\name{getSimInfo}
\alias{getSimInfo}
\title{Get Simulation Information.}
\usage{getSimInfo(obj, problemNum = 1, addRawInfo = TRUE)
}
\description{Extract simulation information from a simulation problem}
\value{A numeric vector with 3 entries : numSimulations, seed1, and seed2. These are self-explanatory.}
\author{Mango Solutions}
\note{If addRawInfo is TRUE, the returned result will have an attribute named 
"rawStatement" that will have the text of the control stream $SIM field.}
\keyword{classes, manip, utilities}
\arguments{
\item{obj}{An object of class NMRun, NMSimDataGen, or NMSimModel}
\item{problemNum}{Number of problem (applicable only when obj is of class NMRun)}
\item{addRawInfo}{Should the raw contents of the $SIM statement be added to the returned information?}
}

