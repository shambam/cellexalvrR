#' Allows the user to select only (G)enes (O)f (I)nterest lists from the object
#' @param cellexalObj A cellexalvr object
#' @param name the name of the GIO list (eg TFs or epigenetic)
#' @export onlyGOIs
onlyGOIs <- function( cellexalObj, name ) {
	if ( is.na( match(name, colnames(cellexalObj@meta.gene)))) {
		stop( "Sorry, but this GIO list not known" )
	}
	cellexalObj = reduceTo(
			cellexalObj, 
			'row', 
			to=rownames(cellexalObj@data)[which(is.na(as.vector(cellexalObj@meta.gene[,name]))==F)] 
	)

	cellexalObj
}