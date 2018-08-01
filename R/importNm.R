

#' Imports a NONMEM run based on a control file and an output list file.  This function assumes that 
#' all of the input and output data tables are in the same directory as the control file, though the input and output
#' tables are allowed to be missing. An object of class \code{NMRun} is returned#' @title Import a NONMEM run
#' @param conFile Single string with control file name
#' @param reportFile The name of the report file.  In the future this parameter will be optional, 
#' in which case importNm will attempt' to deduce the file name on its own based on the name of the conFile
#' and the list file extensions stored in the metadata.
#' @param path The path where the files are located.  Can be the name of a path stored with setNmPath if it
#' is surrounded by round brackets
#' @param dropInputColumns Logical flag.  If TRUE, those columns of the input data flagged for dropping in the 
#' $INPUT statement will not be imported with the run
#' @param textReport Logical flag.  If TRUE, various standard text outputs will be logged to stdReport
#' @return An NMRun object that holds the information of the NONMEM run. If input data tables cannot be imported, 
#' a warning will be emitted and the input data will be ignored.  The same is true if any output table files 
#' cannot be found.
#' @note 
#' The behaviour of this function is fairly complex, due to the intricacies of NONMEM itself. Below we list key
#' behaviours relating to various categories.
#' \itemize{
#'  \item{Input data:}{Input data is specified by the control file $INPUT and $DATA statements. Dropped 
#'      columns will be managed according to the value of the dropInputColumns parameter. Aliased columns will be duplicated, once with their alias, 
#'      and once with their alternative name (e.g. the column SEX=GEN would appear as a column SEX and once as GEN). If there are more columns in the input data than are 
#'      referenced in the $DATA statement they will be attached with the names ExtraCol[x].  Ignored data (as specified by the IGNORE= directive) will not
#'      be available in the returned object. } 
#' \item{Iterations (NONMEM 7 only):}{NONMEM 7-style iteration files (".ext") will be imported as the iterations for a NONMEM 7 run (if available).
#'  The name of the iterations file will be automatically deduced from the control file.  They will be retrievable with the \code{getIterations}
#'  function. } 
#' } 
#' 
#' @author Mango Solutions
#' @keywords IO
#' @export

importNm <- function(conFile, reportFile = NULL, path = NULL, dropInputColumns = TRUE, textReport = FALSE)
{
	
	# collapse the file names to lower case if we are using a windows system
	# TODO: uncomment following verification
	# conFile <- .windowsToLower(conFile)
	# reportFile <- .windowsToLower(reportFile)
	
	
	logMessage(logName = "detailedReport", paste("Importing control file", conFile, "\n"))
	#deal with the 	"path" parameter
	if(!is.null(path))
		path <- processPath(path[1])
        os <- options(stringsAsFactors=FALSE)
        on.exit(options(os))
	# get the FULL paths to both files and store them
	fullConFilePath <- tools::file_path_as_absolute(.getFile(conFile, path))
	conFile <- basename(conFile)
	
	if(!hasExtension(conFile, getNmFileExtensions("control")))
		RNMImportStop("Invalid control file extension for " %pst% conFile, call = match.call())
	
	# if the reportFile was not provided, we will attempt to deduce it.
	if(is.null(reportFile))
	{
		path <- dirname(fullConFilePath)
		conFileVector <- strsplit(conFile, split = "\\.")[[1]]
		allListFiles <- list.files(path)
		whichFile <- sapply(strsplit(allListFiles, split = "\\."), function(x, y)
                {
                    #Condition 1 to test the correct file extension
                    conOne <- casefold(x[length(x)]) %in% getNmFileExtensions("report")
                    #Condition 2 to test the correct names (multiple .s allowed)
					#Condition 3 to test the correct names with the original extensiuon (con) (multiple .s allowed)
					conTwo <- ifelse(length(x) == length(y), 
							all(x[-length(x)] == y[-length(x)]), 
							ifelse(length(x) == length(y) + 1, all(x == c(y,'lst')), FALSE))
                    conOne && conTwo
                }, y = conFileVector)
		reportFile <- allListFiles[whichFile]
		if(!length(reportFile)){
			if(file.exists(file.path(path, 'psn.lst'))){
				RNMImportWarning(msg = "Trying to use psn.lst!")
				reportFile <- 'psn.lst'
			} else {
				RNMImportWarning(msg = "No report file in the directory!")
				return(new('NMRun'))
			}
		}
		if(length(reportFile) > 1)
		{
			RNMImportWarning(msg = paste("More than one report file. Using the first.", reportFile[1]))
			reportFile <- reportFile[1]
		}		
	} else {
		reportFile <- basename(reportFile)
		path <- dirname(fullConFilePath)
	}
	fullLstFilePath <- tools::file_path_as_absolute(.getFile(reportFile, path))
	# read the control file contents
	
	# read in the list file contents.  Note that they should only be omitted in the case of a single SIMONLY run
#	reportContents <- importNmReport(reportFile, path = path, textReport = textReport)
	reportContents <- importNmReport(fileName=reportFile, path = path, controlStatements = NULL, textReport = textReport)
	
	probResults <- reportContents$problemResults
	
	# capture the version
	nmVersionMajor <- reportContents$VersionInfo[1]
	nmVersionMinor <- as.numeric(reportContents$VersionInfo[2])
	versionInfo <- c(nmVersionMajor,  as.character(nmVersionMinor))
	names(versionInfo) <- c("major", "minor")

	# read the control file contents, using the appropriate version
	
	controlContents <- importNmMod(conFile,  path = path, version = versionInfo["major"], 
			textReport = textReport)
	problems <- controlContents$problemContents
	numProblems <- length(problems)
	
	modelList <- vector(mode = "list", length = numProblems)
		
	# iterate through the problems
	for(i in 1:numProblems)
	{
		controlStatements <- problems[[i]]
		if('Tables' %in% names(controlStatements)){
			tableStatements <- controlStatements$Tables
			if(any(tableStatements[,'File']=='npctab.dta')){
                real.dta.name = list.files(path=path, pattern=sprintf('^%s(\\.[1-9])?\\.%s$', sub('\\.[^.]+$','',conFile), 'npctab.dta'))
				sortIt <- which(tableStatements$File=='npctab.dta')
                if (length(real.dta.name)>0) {
                    tableStatements[sortIt,'File'] = real.dta.name[1]
                    if (length(real.dta.name)>1) {
                        tmp0 = tableStatements[sortIt,,drop=FALSE]
                        tmp0$File = NULL
                        tableStatements <- rbind(tableStatements, cbind(File=real.dta.name[-1], tmp0))
                    }
                }
			}
#			Its also possible there is a -cwres statement in the PsN command.txt for NM V or VI
			if(file.exists(file.path(path, 'command.txt'))& nmVersionMajor%in%c('V','VI')){
#			find the possibe cwtabNN file
				cwtab <- list.files(file.path(path), pattern='cwtab[0-9]*$')
				if(length(cwtab)>0){
					tableStatements <- 
							rbind(tableStatements, 
									data.frame(File=cwtab[1], 
											Columns='ID, MDV, DV, IPRE, WRES, CWRES',
											NoHeader=FALSE,
											firstOnly=FALSE,
#           This seems incorrect here, most case the cwtab should not be APPEND
#           However, we handle this in importModelOutputTables
											append=TRUE
									)
							)
#				Now strip the first line off the cwtab file
					con = readLines(file.path(path, cwtab[1]))
					writeLines(con[-1],file.path(path, cwtab[1]))
				}
			}
			controlStatements$Tables <- tableStatements
		}
		reportStatements <- probResults[[i]]
		# check if there is a simulation statement.  If so, proceed accordingly
		if(!is.null(controlStatements$Sim))
		{			
			# there is a simulation statement, so check if it is a "simulation only" run, or a simulation+model fitting run
			isSimOnly <- controlStatements$Sim["simOnly"] == "TRUE"
			if(isSimOnly)
				modelList[[i]] <- NMSimDataGen(controlStatements, path, reportStatements, versionInfo = versionInfo)
			else if(nmVersionMajor == "VII")# this is a simulation+fitting problem
				modelList[[i]] <- NMSimModelNM7(controlStatements, path, reportStatements, versionInfo = versionInfo)
			else
				modelList[[i]] <- NMSimModel(controlStatements, path, reportStatements, versionInfo = versionInfo)
		} # end !is.null(controlStatements$Sim)
		else
		{			 			
			if(nmVersionMajor == "VII")
				modelList[[i]] <- NMBasicModelNM7(controlStatements, path, reportStatements, 
						dropInputColumns = dropInputColumns, versionInfo = versionInfo, conFile=conFile)
			else
				modelList[[i]] <- NMBasicModel(controlStatements, path, reportStatements, 
					dropInputColumns = dropInputColumns, versionInfo = versionInfo)
		}
		# set the subset for graphing - note that this should probably be dropped in the future
		# and replaced with the data subset only.
		
		if(applySubsetOnLoad()) {
			logMessage("Attaching the default subset on loading", logName = "detailedReport")
			graphSubset(modelList[[i]]) <- defaultDataSubset()
			dataSubset(modelList[[i]]) <- defaultDataSubset()
		}
	} # end for(i in 1:numProblems)
	# retrieve basic information on the control and report files, e.g. date of modification, size, etc.
	fileInfo <- file.info(fullConFilePath, fullLstFilePath)
	# obviously these are not directories!
	fileInfo$isdir <- NULL
	fileInfo$fileName <- rownames(fileInfo)
	rownames(fileInfo) <- NULL
	new("NMRun", controlText = controlContents$Raw, 
			controlComments = if(is.null(controlContents$Comments)) character(0) else controlContents$Comments, 
			reportFileInfo = fileInfo[2,],
			controlFileInfo= fileInfo[1,], 
			nmVersionMajor = nmVersionMajor,
			nmVersionMinor = nmVersionMinor,
			numProblems = numProblems, problems = modelList,
			reportText = reportContents$Raw)
}
