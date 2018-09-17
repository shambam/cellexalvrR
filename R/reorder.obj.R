#' @name reorder.samples
#' @aliases reorder.samples,cellexalvr-method
#' @rdname reorder.samples-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the annotation
#' @description  table (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the annotation column to reorder on
#' @title simple reordering of the samples
#' @export reorder.samples
if ( ! isGeneric('reorder.samples') ){setGeneric('reorder.samples', ## Name
	function ( dataObj, column ) { ## Argumente der generischen Funktion
		standardGeneric('reorder.samples') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
) }

setMethod('reorder.samples', signature = c ('cellexalvrR'),
	definition = function ( dataObj, column ) {
	if (! is.na( match ( column, colnames(dataObj@meta.cell)) ) ) {
		ids = order( dataObj@meta.cell[,column])
	}else {
		ids = order( dataObj@userGroups[,column])
	}
	dataObj@data <- dataObj@data[ , ids]
	dataObj@meta.cell <- dataObj@meta.cell[ids,]
	dataObj@userGroups <- dataObj@userGroups[ids,]
	dataObj
} )
#' @name reorder.genes
#' @aliases reorder.genes,cellexalvr-method
#' @rdname reorder.genes-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the samples table
#' @description  (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the samples column to reorder on
#' @title simple reordering of the genes
#' @export reorder.genes
if ( ! isGeneric('reorder.genes') ){setGeneric('reorder.genes', ## Name
	function ( dataObj, column ) { ## Argumente der generischen Funktion
		standardGeneric('reorder.genes') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
) }

setMethod('reorder.genes', signature = c ('cellexalvrR'),
	definition = function ( dataObj, column ) {
	dataObj@data <- dataObj@data[ order( dataObj@meta.gene[,column]),]
	dataObj@meta.gene <- dataObj@meta.gene[order( dataObj@meta.gene[,column]),]
	dataObj
} )
