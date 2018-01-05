
#' Constructor for the NMSimModel class
#' @param controlStatements Set of parsed control statements 
#' @param path Path where the run is located
#' @param reportContents Parsed contents of the report file
#' @return Newly constructed object 
#' @author Mango Solutions
#' @noRd

NMSimModel <- function(controlStatements, path, reportContents, versionInfo = c("major" = "VI", "minor" = 0))
{
    inData <- try(importModelData(dataStatement = controlStatements$Data,inputStatement = controlStatements$Input, path = path))
    # if we could not read data file for some reason, continue anyway
    if(inherits(inData, "try-error"))
    {
        msg <- paste("Could not import data file.  Error generated was:",
                inData, "\nWill continue importing other components\n")
        RNMImportWarning(msg)
        inData <- data.frame()
    } # end if(inherits(inData, "try-error"))

    outTables <- .importTablesSafely(controlStatements$Table, path = path  )

    # if the output tables are a "list", then there was a FIRSTONLY statment, 
    # or for some other reason
    # the number of rows of all of the output tables were not equivalent
    
    if(inherits(outTables, "list")) nDataRows <- max(sapply(outTables, nrow))
    else nDataRows <- nrow(outTables)
    seeds <- as.numeric(ifelse(controlStatements$Sim[c("Seed1", "Seed2")] == -1, NA,    
                    controlStatements$Sim[c("Seed1", "Seed2")]))
    nSim <- as.numeric(controlStatements$Sim["nSub"])
    with(reportContents, 
        {    
            # check how many simulations there are.  
            # If only one, the minimum of the objective funciton is stored
            # differently
            if(nSim == 1) {
                objectiveFinal <- Objective.Minimum
            } else {
                objectiveFinal <- FinalEstimates$Objective.Minimum
            }
            omegaFinal <- FinalEstimates$OMEGA
            # Use parameter labels for names, if any were supplied
            suppliedOmegaNames <- getVariabilityMatrixNames(control = controlStatements, 
                which = "Omega")
            # replace row and column names with OMEGA, or names
            if (!is.null(suppliedOmegaNames)) {
                dimnames(omegaFinal)[1:2] <- suppliedOmegaNames
            }
            thetaFinal <- FinalEstimates$THETA
            colnames(thetaFinal) <- names(controlStatements$Theta[,"Est"])
            sigmaFinal <- FinalEstimates$SIGMA
            if(!is.null(sigmaFinal)) {
                suppliedSigmaNames <- getVariabilityMatrixNames(control = controlStatements, 
                    which = "Sigma")
                if (!is.null(suppliedSigmaNames)) {
                    dimnames(sigmaFinal)[1:2] <- suppliedSigmaNames
                }
            } else {
                sigmaFinal <- array(c(0,0, nSim))
            }
            new("NMSimModel", numSimulations = nSim, 
                    seeds = seeds, inputData = inData, outputData = outTables, controlStatements = 
                            controlStatements, problemStatement = controlStatements$Problem,
                    thetaInitial = controlStatements$Theta[,"Est"], 
                    omegaInitial = getVariabilityMatrix(control = controlStatements, which = "Omega"), 
                    sigmaInitial = getVariabilityMatrix(control = controlStatements, which = "Sigma"),
                    omegaFinal = omegaFinal,
                    sigmaFinal = sigmaFinal,
                    thetaFinal = thetaFinal,
                    objectiveFinal = objectiveFinal,
                    additionalVars = as.data.frame(matrix(ncol = 0, nrow = nDataRows)), 
                    nmVersionMajor = versionInfo["major"],
                    nmVersionMinor = as.numeric(versionInfo["minor"]), 
                    reportStatements = reportContents)
        })
}

# handle control statements with or without intial estimates block
# control$Omega and control$Sigma may be a matrix or list
# if list, extract the initialMatrix block names
# return length 2 list of block names or NULL
# mat <- as.matrix(mtcars[1:4, 1:4])
# getVariabilityMatrix(control = list(Omega = mat))
# getVariabilityMatrix(control = list(Omega = list(initialMatrix = mat)))
# getVariabilityMatrix(control = list(Omega = mat), which = "Sigma")
# getVariabilityMatrixNames(control = list(Omega = mat))
# getVariabilityMatrixNames(control = list(Omega = list(initialMatrix = mat)))
# getVariabilityMatrixNames(control = list(Omega = mat), which = "Sigma")

getVariabilityMatrixNames <- function(control, which = c("Omega", "Sigma")) {
    which <- match.arg(arg = which)
    mat <- getVariabilityMatrix(control = control, which = which)
    dimnames(mat)
}

getVariabilityMatrix <- function(control, which = c("Omega", "Sigma")) {
    which <- match.arg(arg = which)
    switch(class(control[[which]])[1], 
                "matrix" = control[[which]],
                "list" = {
                    mat <- try(control[[which]][["initialMatrix"]])
                    if (is(mat, class2 = "try-error")) { mat <- NULL }
                    mat
                }, 
                {
                    warning(paste("unexpected control statement when getting", 
                        which, "matrix"))
                    NULL
                })
}
