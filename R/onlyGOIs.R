#' @name onlyGOIs
#' @aliases onlyGOIs,cellexalvr-method
#' @rdname onlyGOIs-methods
#' @docType methods
#' @description  Allows the user to select only (G)enes (O)f (I)nterest lists from the object
#' @param cellexalObj A cellexalvr object
#' @param name the name of the GIO list (eg TFs or epigenetic)
#' @title description of function onlyGOIs
#' @export onlyGOIs
setGeneric('onlyGOIs', ## Name
	function ( cellexalObj, name ) { ## Argumente der generischen Funktion
		standardGeneric('onlyGOIs') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('onlyGOIs', signature = c ('cellexalvr'),
		definition = function ( cellexalObj, name ) {
			if ( is.na( match(name, colnames(cellexalObj$meta.gene)))) {
				stop( "Sorry, but this GIO list not known" )
			}
			cellexalObj = reduceTo(
					cellexalObj, 
					'row', 
					to=rownames(cellexalObj$data)[which(as.vector(cellexalObj$meta.gene[,name])==T)] 
			)
			
			cellexalObj
		} 
)
