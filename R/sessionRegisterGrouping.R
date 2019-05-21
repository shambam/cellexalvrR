#' Log function that keeps trach of the number of sections used in the log file.
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
	function ( x, gName ) { 
		standardGeneric('sessionRegisterGrouping')
	}
)

setMethod('sessionRegisterGrouping', signature = c ('cellexalvrR'),
	definition = function ( x, gName ) {
		if ( is.na( match(gName, names(x@usedObj$sessionCounter)) ) ){
			x@usedObj$sessionCounter[[gName]] = length( x@usedObj$sessionCounter) +1
		}
		x
} )
