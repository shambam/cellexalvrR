#' @name sessionRegisterGrouping
#' @aliases sessionRegisterGrouping,cellexalvrR-method
#' @rdname sessionRegisterGrouping-methods
#' @docType methods
#' @description Registers a grouping for this session.
#' @param x the cellexalvrR object
#' @param gName the group name to register
#' @title description of function sessionRegisterGrouping
#' @export 
setGeneric('sessionRegisterGrouping', ## Name
	function ( x, gName ) { ## Argumente der generischen Funktion
		standardGeneric('sessionRegisterGrouping') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('sessionRegisterGrouping', signature = c ('cellexalvrR'),
	definition = function ( x, gName ) {
		x@usedObj$sessionCounter[[gName]] = length( x@usedObj$sessionCounter) +1
		x
} )
