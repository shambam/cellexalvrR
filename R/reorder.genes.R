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
setGeneric('reorder.genes', ## Name
		function ( dataObj, column ) { ## Argumente der generischen Funktion
			standardGeneric('reorder.genes') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('reorder.genes', signature = c ('cellexalvr'),
		definition = function ( dataObj, column ) {
			o <- order( dataObj$meta.gene[,column])
			dataObj$data <- dataObj$data[ o,]
			if ( ! is.null(data$raw)){
				dataObj$raw <- dataObj$raw[ o,]
			}
			if ( ! is.null(data$zscored)){
				dataObj$zscored <- dataObj$zscored[ o,]
			}
			dataObj$meta.gene <- dataObj$meta.gene[ o, ]
			dataObj
		} 
)
