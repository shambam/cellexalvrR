#' @name sessionCounter
#' @aliases sessionCounter,cellexalvrR-method
#' @rdname sessionCounter-methods
#' @docType methods
#' @description 
#' @param x  TEXT MISSING
#' @param gName  TEXT MISSING default=NULL
#' @title description of function sessionCounter
#' @export 
setGeneric('sessionCounter', ## Name
	function ( x, gName=NULL ) { 
		standardGeneric('sessionCounter')
	}
)

setMethod('sessionCounter', signature = c ('cellexalvrR'),
	definition = function ( x, gName=NULL ) {
	x@usedObj$sessionCounter[[gName]]
} )
