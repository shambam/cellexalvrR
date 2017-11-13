#' @name reorder.samples
#' @aliases reorder.samples,cellexalvr-method
#' @rdname reorder.samples-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the annotation
#' @description  table (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the annotation column to reorder on
#' @title description of function remove.genes
#' @export reorder.samples
setGeneric('reorder.samples', ## Name
	function ( dataObj, column ) { ## Argumente der generischen Funktion
		standardGeneric('reorder.samples') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('reorder.samples', signature = c ('cellexalvr'),
	definition = function ( dataObj, column ) {
	if (! is.na( match ( column, colnames(dataObj$meta.cell)) ) ) {
		ids = order( dataObj$meta.cell[,column])
	}else {
		ids = order( dataObj$userGroups[,column])
	}
	dataObj$data <- dataObj$data[ , ids]
	if ( ! is.null(data$raw)){
		dataObj$raw <- dataObj$raw[ , ids]
	}
	if ( ! is.null(data$zscored)){
		dataObj$zscored <- dataObj$zscored[ , ids]
	}
	dataObj$meta.cell <- dataObj$meta.cell[ids,]
	dataObj$userGroups <- dataObj$userGroups[ids,]
	dataObj
} )
