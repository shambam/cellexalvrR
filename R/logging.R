##https://stackoverflow.com/questions/33556266/logging-console-history-with-errors-in-r-or-rstudio

# error handler
#
## All of this has been moved to the .onAttach part in the cellexalvrClass.R file
#
#logErr <- function() {
#	# turn logging callback off while we process errors separately
#	tcbm$suspend(TRUE)
#	# turn them back on when we're done
#	on.exit(tcbm$suspend(FALSE))
#	sc <- sys.calls()
#	sclen <- length(sc)  # last call is this function call
#	if(sclen > 1L) {
#		cat("myError:\n", do.call(paste, c(lapply(sc[-sclen], deparse), sep="\n")), "\n", file="logfile.R", append=T)
#	} else {
#		# syntax error, so no call stack
#		# show the last line entered
#		# (this won't be helpful if it's a parse error in a function)
#		file1 <- tempfile("Rrawhist")
#		savehistory(file1)
#		rawhist <- readLines(file1)
#		unlink(file1)
#		cat("myError:\n", rawhist[length(rawhist)], "\n", file="logfile.R", append=T)
#	}
#}
#options(error=logErr)
## top-level callback handler
#log <- function(expr, value, ok, visible) {
#	cat(deparse(expr), "\n", file="logfile.R", append=T)
#	TRUE
#}
#tcbm <- taskCallbackManager()
#tcbm$add(log, name = "log")
