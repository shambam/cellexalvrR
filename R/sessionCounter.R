
setGeneric('sessionCounter', ## Name
	function ( x, gName ) { 
		standardGeneric('sessionCounter')
	}
)

#' A log helper function. It does remove the need to store all log Rmd files in the object.
#' @name sessionCounter
#' @aliases sessionCounter,cellexalvrR-method
#' @rdname sessionCounter-methods
#' @docType methods
#' @description returns the counter for this session and this group name
#' @param x the cellexalvrR object
#' @param gName the group name
#' @title description of function sessionCounter
#' @export 
setMethod('sessionCounter', signature = c ('cellexalvrR'),
	definition = function ( x, gName ) {
		ret = NA
		if (  is.na( match(gName, names(x@usedObj$sessionCounter)) ) == FALSE ){
			ret = x@usedObj$sessionCounter[[gName]]
		}
		ret
} )
