#' @name force.numeric
#' @aliases force.numeric,cellexalvr-method
#' @rdname force.numeric-methods
#' @docType methods
#' @description make sure, that all variables in the data table are numeric. Add NAs if necessary.
#' @param x the BioData object
#' @title description of function force.numeric
#' @export 
setGeneric('force.numeric', ## Name
		function ( x ) { ## Argumente der generischen Funktion
			standardGeneric('force.numeric') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('force.numeric', signature = c ('cellexalvr'),
		definition = function ( x ) {
			storage.mode(x$data) <- 'numeric'
			
			invisible(x)
		} 
)
