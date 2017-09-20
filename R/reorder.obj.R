
#' @name reorder.samples
#' @aliases reorder.samples,cellexalvr-method
#' @rdname reorder.samples-methods
#' @docType methods
#' @description this function reorderes the cellexalvr object based on a column in the annotation table (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the annotation column to reorder on
#' @title description of function remove.genes
#' @export reorder.samples
reorder.samples <- function ( dataObj, column ) {
	if (! is.na( match ( column, colnames(dataObj@meta.cell)) ) ) {
		ids = order( dataObj@meta.cell[,column])
	}else {
		ids = order( dataObj@userGroups[,column])
	}
	dataObj@data <- dataObj@data[ , ids]
	dataObj@meta.cell <- dataObj@meta.cell[ids,]
	dataObj@userGroups <- dataObj@userGroups[ids,]
	dataObj
}


#' @name reorder.genes
#' @aliases reorder.genes,cellexalvr-method
#' @rdname reorder.genes-methods
#' @docType methods
#' @description this function reorderes the cellexalvr object based on a column in the samples table (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the samples column to reorder on
#' @title description of function remove.genes
#' @export reorder.genes
reorder.genes = function ( dataObj, column ) {
	dataObj@data <- dataObj@data[ order( dataObj@meta.gene[,column]),]
	dataObj@meta.gene <- dataObj@meta.gene[order( dataObj@meta.gene[,column]),]
	dataObj
}

