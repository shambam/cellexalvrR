#' @name changeIdent
#' @aliases changeIdent,cellexalvr-method
#' @rdname changeIdent-methods
#' @docType methods
#' @description Adds new cell identities to the seurat object
#' @param seuratObj The seurat object to be changed
#' @param new.idents The new cell names
#' @title description of function changeIdent
#' @export 
if ( ! isGeneric('changeIdent') ){ setGeneric('changeIdent', ## Name
	function (seuratObj,new.idents) { ## Argumente der generischen Funktion
		standardGeneric('changeIdent') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)
}else {
	print ("Onload warn generic function 'changeIdent' already defined - no overloading here!")
}

setMethod('changeIdent', signature = c ('seurat'),
	definition = function (seuratObj,new.idents) {

    new.ids <- rep("",length(seuratObj@cell.names))

    for(i in 1:length(new.idents)){

        new.ids[grep(as.character(-i),seuratObj@cell.names)] <- new.idents[i]

    }
    seuratObj<- SetIdent(seuratObj,ident.use=new.ids)
    seuratObj
} )
