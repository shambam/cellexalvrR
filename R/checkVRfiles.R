#' @name checkVRfiles
#' @aliases checkVRfiles,cellexalvr-method
#' @rdname checkVRfiles-methods
#' @docType methods
#' @description  checkVRfiles: Checks the existance of all VR specific files and re-runs the export
#' @description  function if any is missing.
#' @param cellexalObj the cellexal object
#' @param path the outpath to check
#' @title description of function checkVRfiles
#' @export checkVRfiles
setGeneric('checkVRfiles', ## Name
	function ( cellexalvr, path ) { ## Argumente der generischen Funktion
		standardGeneric('checkVRfiles') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('checkVRfiles', signature = c ('cellexalvr'),
	definition = function ( cellexalvr, path ) {
	export2cellexalvr( cellexalvr, path, forceDB=F )
} )
