#' @name loadObject
#' @aliases loadObject,cellexalvr-method
#' @rdname loadObject-methods
#' @docType methods
#' @description loads a cellexalvr object fron a file, but waits on 
#' all systems before another process has finished writing the file
#' unsing the lockedSave() function.
#' @param fname the file to load
#' @param maxwait  TEXT MISSING default=50
#' @title description of function loadObject
#' @export 
setGeneric('loadObject', ## Name
	function ( fname, maxwait=50 ) { ## Argumente der generischen Funktion
		standardGeneric('loadObject') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('loadObject', signature = c ('cellexalvr'),
		definition = function ( fname, maxwait=50 ) {
			fname
		}
)

setMethod('loadObject', signature = c ('character'),
		definition = function ( fname, maxwait=50 ) {
			
			if ( file.exists( fname) ) {
				waited = 0
				while ( file.exists( paste(fname, 'lock',sep='.'))){
					Sys.sleep(1)
					waited = waited +1
					if ( waited == maxwait) { break }
				}
				if (waited != maxwait ){
					load(fname)
				}else {
					stop( paste("Could not obtain access to locked file", fname ))
				}
			}
			
			cellexalObj
		} 
)
