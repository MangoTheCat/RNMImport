
## Test suite designed to test the importNmModSub function ##

test.importNmModSub <- function() {
          
	checkEquals( RNMImport:::.importNmModSub(" A "), "A")
	checkEquals( RNMImport:::.importNmModSub(" A B "), c("A", "B"))
	checkEquals( RNMImport:::.importNmModSub(c(" A B ", " C ")), c("A", "B", "C"))
	checkEquals( RNMImport:::.importNmModSub(c(" A B ", " C ", " D = E")), c("A", "B", "C", "D=E"))
	checkEquals( RNMImport:::.importNmModSub(c(" A B ", " C ", " D = E TOL=1")), c("A", "B", "C", "D=E"))
	checkEquals( RNMImport:::.importNmModSub(c(" A B ", " C ", " D = E SUBROUTINES=qwqq")), c("A", "B", "C", "D=E"))
	checkEquals( RNMImport:::.importNmModSub(c(" A B TOL = 9 SUBROUTINES  = 5", " C ", " D = E")), c("A", "B", "C", "D=E"))

}

