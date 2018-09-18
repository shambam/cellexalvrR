#' @name lockedSave
#' @aliases lockedSave,cellexalvrR-method
#' @rdname lockedSave-methods
#' @docType methods
#' @description  Saving the RData in the VR tool might create a problem. Hence this function will
#' @description  save the cellexalObj in a controlled way.
#' @param cellexalObj A cellexalvr object
#' @param path the output path
#' @param path  TEXT MISSING
#' @title description of function lockedSave
#' @keywords lockedSave
#' @export lockedSave
if ( ! isGeneric('lockedSave') ){setGeneric('lockedSave', ## Name
	function (cellexalObj, path ) { 
		standardGeneric('lockedSave') 
	}
) }

setMethod('lockedSave', signature = c ('cellexalvrR'),
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
} )
