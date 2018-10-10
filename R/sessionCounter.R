#' @name sessionCounter
#' @aliases sessionCounter,cellexalvrR-method
#' @rdname sessionCounter-methods
#' @docType methods
#' @description returns the counter for this session and this group name
#' @param x the cellexalvrR object
#' @param gName the group name
#' @title description of function sessionCounter
#' @export 
setGeneric('sessionCounter', ## Name
	function ( x, gName ) { ## Argumente der generischen Funktion
		standardGeneric('sessionCounter') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('sessionCounter', signature = c ('cellexalvrR'),
	definition = function ( x, gName ) {
	x@usedObj$sessionCounter[[gName]]
} )
