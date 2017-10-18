#' @name reorder.genes
#' @aliases reorder.genes,cellexalvr-method
#' @rdname reorder.genes-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the samples table
#' @description  (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the samples column to reorder on
#' @title description of function remove.genes
#' @export reorder.genes
if ( ! isGeneric('reorder.genes') ){ setGeneric('reorder.genes', ## Name
		function ( dataObj, column ) { ## Argumente der generischen Funktion
			standardGeneric('reorder.genes') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)
}else {
	print ("Onload warn generic function 'reorder.genes' already defined - no overloading here!")
}

setMethod('reorder.genes', signature = c ('cellexalvr'),
		definition = function ( dataObj, column ) {
			dataObj$data <- dataObj$data[ order( dataObj$meta.gene[,column]),]
			dataObj$meta.gene <- dataObj$meta.gene[order( dataObj$meta.gene[,column]),]
			dataObj
		} 
)
