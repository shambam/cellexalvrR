#' @name onlyGOIs
#' @aliases onlyGOIs,cellexalvr-method
#' @rdname onlyGOIs-methods
#' @docType methods
#' @description  Allows the user to select only (G)enes (O)f (I)nterest lists from the object
#' @param cellexalObj A cellexalvr object
#' @param name the name of the GIO list (eg TFs or epigenetic)
#' @param copy create a copy of the cellexalObj to not modify it (default = TRUE)
#' @title description of function onlyGOIs
#' @export onlyGOIs
if ( ! isGeneric('onlyGOIs') ){ setGeneric('onlyGOIs', ## Name
	function ( cellexalObj, name, copy=TRUE ) { ## Argumente der generischen Funktion
		standardGeneric('onlyGOIs') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)
}else {
	print ("Onload warn generic function 'onlyGOIs' already defined - no overloading here!")
}

setMethod('onlyGOIs', signature = c ('cellexalvr'),
		definition = function ( cellexalObj, name, copy=TRUE  ) {
			if ( is.na( match(name, colnames(cellexalObj$meta.gene)))) {
				stop( "Sorry, but this GIO list not known" )
			}
			cellexalObj = reduceTo(
					cellexalObj, 
					'row', 
					to=rownames(cellexalObj$data)[which(is.na(as.vector(cellexalObj$meta.gene[,name]))==F)],
					copy=copy
			)
			
			cellexalObj
		} 
)
