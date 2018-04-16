#'Adds mds coordinates to a cellexalvrObj
#'@param cellexalObj A cellexalvr object
#'@param mdsmatrix A matrix of coordinates
#' @param name A name for the object (default = graph<n>)
#'@export addMDS2cellexalvr
addMDS2cellexalvr <- function(cellexalObj,mdsmatrix, name=NULL){

    rq.ind <- (length(cellexalObj@mds)+1)
	if ( ! is.null(name) ){
		rq.nm <- name
	}else {
    	rq.nm <- paste("graph",(length(cellexalObj@mds)+1),sep="")
	}
    mp <- mdsmatrix
    colnames(mp) <- c("x","y","z")
    rownames(mp) <- colnames(cellexalObj@data)

    cellexalObj@mds[[rq.ind]] <- mp
    names(cellexalObj@mds)[rq.ind] <- rq.nm
    cellexalObj
}

#'Adds mds coordinates to a cellexalvrObj
#'@param cellexalObj A cellexalvr object
#'@param facs.data A matrix of coordinates)
#'@export addFACS2cellexalvr

addFACS2cellexalvr <- function(cellexalObj,facs.data){

    rownames(facs.data) <- colnames(cellexalObj@data)
    cellexalObj@index <- as.matrix(facs.data)
    cellexalObj
}