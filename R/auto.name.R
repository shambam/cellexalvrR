#' @name auto_name
#' @aliases auto_name,cellexalvr-method
#' @rdname auto_name-methods
#' @docType methods
#' @description This is a simple (internal) function to return a new name if the user has not given a name for a grouing.
#' @param cellexalObj the object
#' @param new want the last name or a new name default=TRUE (new)
#' @title description of function auto.name
#' @export 
setGeneric('auto_name', ## Name
	function ( cellexalObj, new=TRUE ) { ## Argumente der generischen Funktion
		standardGeneric('auto_name') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('auto_name', signature = c ('cellexalvr'),
	definition = function ( cellexalObj, new=TRUE ) {
	if ( is.null(cellexalObj$usedObj$autoID) ) {
		cellexalObj$usedObj$autoID = 1
	}else if ( new ) {
		cellexalObj$usedObj$autoID = cellexalObj$usedObj$autoID +1
	}
	 paste( "Grouping_Nr.", cellexalObj$usedObj$autoID , sep="" )
} )
