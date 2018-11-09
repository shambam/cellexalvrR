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
	function ( x, gName ) { 
		standardGeneric('sessionCounter')
	}
)

setMethod('sessionCounter', signature = c ('cellexalvrR'),
	definition = function ( x, gName ) {
		ret = NA
		if (  is.na( match(gName, names(x@usedObj$sessionCounter)) ) == FALSE ){
			ret = x@usedObj$sessionCounter[[gName]]
		}
		ret
} )
