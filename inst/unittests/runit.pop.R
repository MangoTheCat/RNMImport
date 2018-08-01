# 
# Author: fgochez
# Unit tests for pop, based on R. Francois's code in RNONMEM
###############################################################################

###############################################################################
# tests "pop", code by F. Gochez and R. Francois
###############################################################################




test.pop <- function(){
	pop <- RNMImport:::pop
	# (logical mode)
	x <- "grewge frewfr gtrseggtrr PRINT"
	
	out <-  pop( x, "PRINT", mode = "logical", inPlace = FALSE)
	checkTrue( out$op.out,	msg = "equal pop (1)")
	checkTrue( length( grep("PRINT", x )) == 1,
			msg = "checking that the option is NOT out of the string")
	checkEquals(out$txt, "grewge frewfr gtrseggtrr ")	
	#
	x <- "grewge frewfr gtrseggtrr PRINT"
			
	out <-  pop( x, "PRINT", mode = "logical", inPlace = TRUE)
	checkTrue( out,
			msg = "equal pop (1)")
	checkTrue( length( grep("PRINT", x )) ==0,
			msg = "checking that the option is taken out of the string")
	
	
	x <- "grewge frewfr gtrseggtrr"
	out <- pop( x, "PRINT", mode = "logical", inPlace = TRUE )
	checkTrue( !out,
			msg = "equal pop (missing default)")
	
	# bracket pop
	x <- "vt yehe hyr jut (154) fr3f34 (123)"
	out <- pop( x, mode = "bracket", removeBrackets = TRUE, inPlace = FALSE)
	checkEquals( out$op.out, "154",
			msg = "pop bracket removing brackets")
	checkEquals(x, "vt yehe hyr jut (154) fr3f34 (123)")
	out <- pop( out$txt, mode = "bracket", removeBrackets = FALSE, inPlace = FALSE)
	checkEquals( out$op.out, "(123)",
			msg = "pop bracket not removing brackets")
	
	# comments pop
	x <- c("vt; yehe hyr jut (154) fr3f34 (123) ", "vgt4 gtet4 hy5e5 hr5 h65e5", "gfrew gwer; gewr " )
	out <- pop( x , mode = "comments", inPlace = TRUE)
	checkTrue( all(regexpr(x, ";")==-1))
	checkEquals( out[1], " yehe hyr jut (154) fr3f34 (123) " )
	checkEquals( out[2], "" )
	checkEquals( out[3], " gewr " )
	
	# equal pop
	x <- "OPT=3"
	out <- pop(x, "OPT", mode = "equal", inPlace = FALSE)
	checkEquals( out$txt, " ")
	checkEquals( out$op.out, "3")
	
	# equal pop
	x <- "OPT (3)"
	out <- pop(x, "OPT", mode = "equal", sep = "[[:space:]]+", inPlace = TRUE)
	checkEquals( x, " ")
	checkEquals( out, "(3)")

}

###############################################################################
# tests "equalExppressionPop", code by F. Gochez and R. Francois
###############################################################################

test.equalExpressionPop <- function(){
	equalExpressionPop <- RNMImport:::equalExpressionPop
	x <- "OPT=3"
	out <- equalExpressionPop(x, "OPT", inPlace = TRUE)
	checkEquals( x, " ")
	checkEquals( out, "3")
	
	# equal pop
	x <- "OPT (3)"
	out <- equalExpressionPop(x, "OPT", sep = "[[:space:]]+", inPlace = FALSE)
	checkEquals( out$txt, " ")
	checkEquals( out$op.out, "(3)")

	### incorporating code previously found under "test.espop"
	
	x <- "OPT=3"
	out <- equalExpressionPop(x, "OPT", inPlace = TRUE)
	checkEquals( x, " ")
	checkEquals( out, "3")
	
	x <- "OPT  =3"
	out <- equalExpressionPop(x, "OPT", inPlace = TRUE)
	checkEquals( x, " ")
	checkEquals( out, "3")
	
	x <- "OPT=  3"
	out <- equalExpressionPop(x, "OPT", inPlace = FALSE)
	checkEquals( out$txt, " ")
	checkEquals( out$op.out, "3")
	
	x <- "OPT  =  3  "
	out <- equalExpressionPop(x, "OPT", inPlace = TRUE)
	checkEquals( x, " ")
	checkEquals( out, "3")
}

###############################################################################
# tests "bracketPop", code by F. Gochez and R. Francois
###############################################################################


test.bracketPop <- function(){
	bracketPop <- RNMImport:::bracketPop
	x <- "vt yehe hyr jut (154) fr3f34 (123)"
	out <- bracketPop( x, removeBrackets = TRUE, inPlace = TRUE)
	checkEquals( out, "154",
			msg = "pop bracket removing brackets")
	out <- bracketPop( x, removeBrackets = FALSE, inPlace = FALSE)
	checkEquals( out$op.out, "(123)",
			msg = "pop bracket not removing brackets")
}

test.logicalPop <- function(){
	logicalPop <- RNMImport:::logicalPop
	x <- "grewge frewfr gtrseggtrr PRINT"
	out <-  logicalPop( x, "PRINT", inPlace = FALSE)
	checkTrue( out$op.out,
			msg = "equal pop (1)")
	checkTrue( length(grep("PRINT", out$txt )) == 0,
			msg = "checking that the option is taken out of the string")
	
	out <- logicalPop( out$txt, "PRINT", inPlace = FALSE)
	checkTrue( !out$op.out,
			msg = "equal pop (missing default)")
	
}

test.commentPop <- function(){
	commentPop <- RNMImport:::commentPop
	x <- c("vt; yehe hyr jut (154) fr3f34 (123) ", "vgt4 gtet4 hy5e5 hr5 h65e5", "gfrew gwer; gewr " )
	out <- commentPop( x, inPlace = TRUE )
	checkTrue( all(regexpr(x, ";")==-1))
	checkEquals( out[1], " yehe hyr jut (154) fr3f34 (123) " )
	checkEquals( out[2], "" )
	checkEquals( out[3], " gewr " )
	
}

test.ynPop <- function(){
	ynPop <- RNMImport:::ynPop
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 REWIND"
	rewind <- ynPop(x, "REWIND", inPlace = TRUE)
	checkTrue( rewind )
	checkEquals(x, "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 ")
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 REWIND"
	rewind <- ynPop(x, "REWIND|NOREWIND", inPlace = FALSE)
	checkTrue( rewind)
	checkEquals(x, "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 REWIND")
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 REWIND"
	rewind <- ynPop(x, "NOREWIND|REWIND", inPlace = TRUE)
	checkTrue( rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 NOREWIND"
	rewind <- ynPop(x, "REWIND", inPlace = TRUE)
	checkTrue( !rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 NOREWIND"
	rewind <- ynPop(x, "REWIND|NOREWIND", inPlace = FALSE)
	checkTrue( !rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 NOREWIND"
	rewind <- ynPop(x, "NOREWIND|REWIND", inPlace = TRUE)
	checkTrue( !rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543"
	rewind <- ynPop(x, "REWIND", inPlace = TRUE)
	checkTrue( rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543D"
	rewind <- ynPop(x, "REWIND|NOREWIND", inPlace = FALSE)
	checkTrue( rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543ND"
	rewind <- ynPop(x, "NOREWIND|REWIND", inPlace = TRUE)
	checkTrue( !rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 REWIND NOREWIND"
	rewind <- ynPop(x, "REWIND", inPlace = TRUE)
	checkTrue( rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543D REWIND"
	rewind <- ynPop(x, "REWIND|NOREWIND", inPlace = TRUE)
	checkTrue( rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543ND NOREWIND"
	rewind <- ynPop(x, "NOREWIND|REWIND", inPlace = TRUE)
	checkTrue( !rewind )
	
	x <- "vtw hweyhte hte htehtrehtehyet hy63 by543 y54 y543 REWIND"
	rewind <- ynPop(x, "REWIND", default = FALSE, inPlace = FALSE)
	checkTrue( rewind )
}
