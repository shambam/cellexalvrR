#' @name as_cellexalvr
#' @aliases as_cellexalvr,cellexalvr-method
#' @rdname as_cellexalvr-methods
#' @docType methods
#' @description 
#' @param x  TEXT MISSING
#' @title description of function as_cellexalvr
#' @export 
setGeneric('as_cellexalvr', ## Name
	function ( x ) { ## Argumente der generischen Funktion
		standardGeneric('as_cellexalvr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('as_cellexalvr', signature = c ('cellexalvr'),
	definition = function ( x ) {
	## do all the things you need to do!:x
}  )
