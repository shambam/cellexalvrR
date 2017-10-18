#' @name run.ddrtree
#' @aliases run.ddrtree,cellexalvr-method
#' @rdname run.ddrtree-methods
#' @docType methods
#' @description creates a DDRTree 3D matrix from a seurat object using only the variable genes.
#' @param seuratObj a seurat object
#' @title description of function run.ddrtree
#' @export 
if ( ! isGeneric('run.ddrtree') ){ setGeneric('run.ddrtree', ## Name
	function (seuratObj) { ## Argumente der generischen Funktion
		standardGeneric('run.ddrtree') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)
}else {
	print ("Onload warn generic function 'run.ddrtree' already defined - no overloading here!")
}

setMethod('run.ddrtree', signature = c ('seurat'),
	definition = function (seuratObj) {

    dat.samp <- as.matrix(seuratObj@data[seuratObj@var.genes,])
    ddr.samp <- DDRTree((dat.samp), dimensions=3)
    ddr.cood <- t(ddr.samp$Z)
    ddr.cood    
} )
