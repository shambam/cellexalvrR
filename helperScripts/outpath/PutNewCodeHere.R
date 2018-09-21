#' @name rmdLink
#' @aliases rmdLink,cellexalvrR-method
#' @rdname rmdLink-methods
#' @docType methods
#' @description 
#' @param name  TEXT MISSING
#' @param link  TEXT MISSING
#' @title description of function rmdLink
#' @export 
if ( ! isGeneric('rmdLink') ){setGeneric('rmdLink', ## Name
	function ( name, link ) { ## Argumente der generischen Funktion
		standardGeneric('rmdLink') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
) }

setMethod('rmdLink', signature = c ('cellexalvrR'),
	definition = function ( name, link ) {
	paste( sep="", "[", name,"](",link,")[target='blank']")
} )
