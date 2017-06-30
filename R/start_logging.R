#' @name start_logging
#' @aliases start_logging,cellexalvr-method
#' @rdname start_logging-methods
#' @docType methods
#' @description creates a taskCallbackManager in x$usedObj$tcbm which hopefully loggs all additional changes
#' @param x  TEXT MISSING
#' @title description of function start_logging
#' @export 
setGeneric('start_logging', ## Name
	function ( x ) { ## Argumente der generischen Funktion
		standardGeneric('start_logging') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('start_logging', signature = c ('cellexalvr'),
	definition = function ( x ) {
	
	
	##https://stackoverflow.com/questions/33556266/logging-console-history-with-errors-in-r-or-rstudio
	
# error handler
	## this function needs to be more generalized! Put it in the R6 class definition? 
	logErr <- function() {
		# turn logging callback off while we process errors separately
		x$usedObj$tcbm$suspend(TRUE)
		# turn them back on when we're done
		on.exit(x$usedObj$tcbm$suspend(FALSE))
		sc <- sys.calls()
		sclen <- length(sc)  # last call is this function call
		if(sclen > 1L) {
			cat("myError:\n", do.call(paste, c(lapply(sc[-sclen], deparse), sep="\n")), "\n", file="logfile.R", append=T)
		} else {
			# syntax error, so no call stack
			# show the last line entered
			# (this won't be helpful if it's a parse error in a function)
			file1 <- tempfile("Rrawhist")
			savehistory(file1)
			rawhist <- readLines(file1)
			unlink(file1)
			cat("myError:\n", rawhist[length(rawhist)], "\n", file="logfile.R", append=T)
		}
	}
# top-level callback handler
	log <- function(expr, value, ok, visible) {
		cat(deparse(expr), "\n", file="logfile.R", append=T)
		TRUE
	}
	if ( is.null( x$usedObj$tcbm )){
		options(error=logErr)
		x$usedObj$tcbm <- taskCallbackManager()
		x$usedObj$tcbm$add(log, name = "log")
	}
	invisible(x)
} )
