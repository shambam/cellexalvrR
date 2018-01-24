#' @name as_cellexalvr
#' @aliases as_cellexalvr,cellexalvr-method
#' @rdname as_cellexalvr-methods
#' @docType methods
#' @description Convert a R6::BioData object into a R6::cellexalvr object
#' @param x the BioData object
#' @title description of function as_cellexalvr
#' @export 
setGeneric('as_cellexalvr', ## Name
	function ( x ) { ## Argumente der generischen Funktion
		standardGeneric('as_cellexalvr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('as_cellexalvr', signature = c ('BioData'),
	definition = function ( x ) {
		if ( is.null(x$usedObj$MDS) ){
			x$usedObj$MDS <- list()
		}
		ret = cellexalvr$new(x$dat,meta.cell=x$samples, meta.gene=x$annotation, mds=x$usedObj$MDS,index = NULL  )
		ret
}  )
