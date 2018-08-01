\name{runRNMImportTests}
\alias{runRNMImportTests}
\title{run package regression tests}
\usage{
runRNMImportTests(TestPath = system.file(package = "RNMImport", "unittests"),
  ExcludeFolders = NULL, TestResult = NULL, ResultsType = c("html",
  "text"))
}
\arguments{
  \item{TestPath}{Path of the folder which contains the
  test scripts.}

  \item{ExcludeFolders}{The folders are not tested.}

  \item{TestResult}{Name of the report file.}

  \item{ResultsType}{'html' or 'text'.}
}
\value{
The results of function \code{runTestSuite} of \code{RUnit}.
}
\description{
Executes the package regression tests, and optionally produces one or two html reports. The test suite is run with the RUnit package.
}
\details{
Run the unit tests by RUnit package, and generate a html or
text report.
}
\examples{
\dontrun{
x <- runRNMImportTests()
summary(x)
}
}
\author{
Mango Solutions
}
\keyword{debugging}

