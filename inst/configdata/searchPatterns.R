# TODO: Add comment
# 
# Author: jjames
###############################################################################
#(.*) #GSK
#([^~[:space:]]+) #Others
assign('.patterns',
		list(
				THETAPATTERN='(.*)',
				OMEGAPATTERN='(.*)',
				SIGMAPATTERN='(.*)',
				PRIORPATTERN='(.*)',
				SIGDIGS71='Sig.digs|SIG. DIGITS'
		)
)

assign('.parameters',
		list(
				startHandlingSims=10299544 ,# This sets the minimum siz for splitting large (SIM) files into chuncks
				readLinesIn=-1 # reads all the data - can be set much smaller 0.01-0.1 (*100 as percent) of lines for debug purposes
		)
)
##RNMImport:::.RNMImportEnv)

