#' @name renew
#' @aliases renew,cellexalvr-method
#' @rdname renew-methods
#' @docType methods
#' @description update the class definition by re-creating the instance
#' @param x the object you want to update
#' @title description of function renew
#' @export 
setGeneric('renew', ## Name
	function ( x ) { ## Argumente der generischen Funktion
		standardGeneric('renew') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('renew', signature = c ('cellexalvr'),
	definition = function ( x ) {
	ret <- cellexalvr$new( 
			dat = x$data,
			meta.gene = x$meta.gene, 
			meta.cell = x$meta.cell, 
			name=x$name
	)
	ret$usedObj <- x$usedObj
	ret$stats <- x$stats
	x <- ret
	invisible(x)
} )
