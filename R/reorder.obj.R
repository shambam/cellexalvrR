#' @name reorder.samples
#' @aliases reorder.samples,cellexalvrR-method
#' @rdname reorder.samples-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the annotation
#' @description  table (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the annotation column to reorder on
#' @title simple reordering of the samples
#' @export
if ( ! isGeneric('reorder.samples') ){setGeneric('reorder.samples', ## Name
	function ( dataObj, column ) { 
		standardGeneric('reorder.samples') 
	}
) }

setMethod('reorder.samples', signature = c ('cellexalvrR'),
	definition = function ( dataObj, column ) {
	if (! is.na( match ( column, colnames(dataObj@meta.cell)) ) ) {
		ids = order( dataObj@meta.cell[,column])
	}else {
		ids = order( dataObj@userGroups[,column])
	}
	dataObj@dat <- dataObj@dat[ , ids]
	if ( ncol(dataObj@dat) == nrow(dataObj@meta.cell) ) {
		dataObj@meta.cell <- dataObj@meta.cell[ids,]
	}
	if ( ncol(dataObj@dat) == nrow(dataObj@userGroups) ) {
		dataObj@userGroups <- dataObj@userGroups[ids,]
	}
	dataObj
} )
#' @name reorder.genes
#' @aliases reorder.genes,cellexalvrR-method
#' @rdname reorder.genes-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the samples table
#' @description  (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the samples column to reorder on
#' @title simple reordering of the genes
#' @export
if ( ! isGeneric('reorder.genes') ){setGeneric('reorder.genes', ## Name
	function ( dataObj, column ) { 
		standardGeneric('reorder.genes') 
	}
) }

setMethod('reorder.genes', signature = c ('cellexalvrR'),
	definition = function ( dataObj, column ) {
	dataObj@dat <- dataObj@dat[ order( dataObj@meta.gene[,column]),]
	dataObj@meta.gene <- dataObj@meta.gene[order( dataObj@meta.gene[,column]),]
	dataObj
} )
