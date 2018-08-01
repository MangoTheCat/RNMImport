### R code from vignette source 'Notes_on_RNMImport.Rnw'

###################################################
### code chunk number 1: Notes_on_RNMImport.Rnw:34-35
###################################################
options(width=70)


###################################################
### code chunk number 2: Notes_on_RNMImport.Rnw:38-39
###################################################
require(RNMImport)


###################################################
### code chunk number 3: Notes_on_RNMImport.Rnw:47-53
###################################################
# Import an example run
runPath <- system.file(package = "RNMImport", "examples/theoph")
# List file deduced automatically
run <- importNm(conFile = "theoph.con", path = runPath)
print(run)
print(class(run))


###################################################
### code chunk number 4: Notes_on_RNMImport.Rnw:58-59
###################################################
print(getClass("NMRun"))


###################################################
### code chunk number 5: Notes_on_RNMImport.Rnw:65-67
###################################################
prob <- getProblem(run)
print(prob)


###################################################
### code chunk number 6: Notes_on_RNMImport.Rnw:99-101
###################################################
print(getThetas(prob))
print(getOmegas(prob))


###################################################
### code chunk number 7: Notes_on_RNMImport.Rnw:111-114
###################################################
print(getThetas(run, problemNum = 1))
print(getOmegas(prob, problemNum = 1))



###################################################
### code chunk number 8: Notes_on_RNMImport.Rnw:131-135
###################################################
probOutData <- nmData(prob, dataTypes = "output")
print(head(probOutData))
probData <- nmData(prob)
print(head(probData))


###################################################
### code chunk number 9: Notes_on_RNMImport.Rnw:149-151
###################################################
x <- nmDatabyVarType(run, varTypes = "Parameter,Covariate", problemNum = 1 )
print(head(x))


###################################################
### code chunk number 10: Notes_on_RNMImport.Rnw:155-159
###################################################
prob <- getProblem(run)
prob <- addDerivedCategorical(prob, "IRES", "IRES.CUT", 
    breaks = 3, labels = c("low", "medium", "high"))
print(head(addedData(prob)))


###################################################
### code chunk number 11: Notes_on_RNMImport.Rnw:165-171
###################################################
print(runPath)
setNmPath("runPath", runPath)
# note the use of round brackets
controlContents <- importNmMod("theoph.con", path = "(runPath)" )
print(head(controlContents))
removeNmPath("runPath")


###################################################
### code chunk number 12: Notes_on_RNMImport.Rnw:177-183
###################################################
print(getVarDescription(c("SEX", "SMOK")))
setVarDescription("SMOK", "Smokes", varFormat = "0=NO, 
    1 = YES", varType = "Covariate")
dat <- nmData(prob)
dat <- imposeCategoryFormat(dat, varSubset = "SMOK")
print(head(dat))


