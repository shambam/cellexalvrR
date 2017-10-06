#' @name addMDS2cellexalvr
#' @aliases addMDS2cellexalvr,cellexalvr-method
#' @rdname addMDS2cellexalvr-methods
#' @docType methods
#' @description Adds an externally created MDS to this object
#' @param name A name for the object (default = graph<n>)
#' @param cellexalObj the cellexalvr object
#' @param mdsmatrix the mds 3D matrix
#' @param name the name of this MDS e.g. 'PCA' default=NULL
#' @title description of function addMDS2cellexalvr
#' @export 
setGeneric('addMDS2cellexalvr', ## Name
	function (cellexalObj,mdsmatrix, name=NULL) { ## Argumente der generischen Funktion
		standardGeneric('addMDS2cellexalvr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('addMDS2cellexalvr', signature = c ('cellexalvr'),
	definition = function (cellexalObj,mdsmatrix, name=NULL) {

    rq.ind <- (length(cellexalObj$mds)+1)
	if ( ! is.null(name) ){
		rq.nm <- name
	}else {
    	rq.nm <- paste("graph",(length(cellexalObj$mds)+1),sep="")
	}
    mp <- mdsmatrix
    colnames(mp) <- c("x","y","z")
    rownames(mp) <- colnames(cellexalObj$data)

    cellexalObj$mds[[rq.ind]] <- mp
    names(cellexalObj$mds)[rq.ind] <- rq.nm
    cellexalObj
} )
