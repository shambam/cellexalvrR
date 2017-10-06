#' @name lockedSave
#' @aliases lockedSave,cellexalvr-method
#' @rdname lockedSave-methods
#' @docType methods
#' @description  Saving the RData in the VR tool might create a problem. Hence this function will
#' @description  save the cellexalObj in a controlled way.
#' @param cellexalObj  TEXT MISSING
#' @param path  TEXT MISSING
#' @title description of function lockedSave
#' @export 
setGeneric('lockedSave', ## Name
	function (cellexalObj, path ) { ## Argumente der generischen Funktion
		standardGeneric('lockedSave') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('lockedSave', signature = c ('cellexalvr'),
		definition = function (cellexalObj, path ) {
			ofile = file.path( path, 'cellexalObj.RData' )
			lockFile = file.path( paste(ofile, 'lock', sep= '.'))
			while ( file.exists(lockFile) ){
				Sys.sleep(1)
			}
			file.create(lockFile)
			save(cellexalObj, file=ofile)
			file.remove(lockFile)
			print (paste("saved the object to",path))
		} 
)
