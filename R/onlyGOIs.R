#' @name onlyGOIs
#' @aliases onlyGOIs,cellexalvrR-method
#' @rdname onlyGOIs-methods
#' @docType methods
#' @description  Allows the user to select only (G)enes (O)f (I)nterest lists from the object
#' @param cellexalObj, cellexalvr object
#' @param name the name of the GIO list (eg TFs or epigenetic)
#' @param cellexalObj, TEXT MISSING
#' @param name  TEXT MISSING
#' @title description of function onlyGOIs
#' @export onlyGOIs
if ( ! isGeneric('onlyGOIs') ){setGeneric('onlyGOIs', ## Name
	function ( cellexalObj, name ) { 
		standardGeneric('onlyGOIs') 
	}
) }

setMethod('onlyGOIs', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, name ) {
	if ( is.na( match(name, colnames(cellexalObj@meta.gene)))) {
		stop( "Sorry, but this GIO list not known" )
	}
	cellexalObj = reduceTo(
			cellexalObj, 
			'row', 
			to=rownames(cellexalObj@data)[which(is.na(as.vector(cellexalObj@meta.gene[,name]))==F)] 
	)

	cellexalObj
} )
