#' This function can convert a 'Seurat' (v <3.0.0) object into 'cellexalvrR'.
#' @name seurat2cellexalvr
#' @aliases seurat2cellexalvr,seurat-method
#' @rdname seurat2cellexalvr-methods
#' @docType methods
#' @description  Converts a 'seurat' class to one of 'cellexalvr'. If the cell-cycle score have been calculated
#' @description  they will be added to the cell.mata table
#' @param seuratObj the suerat object to be converted
#' @title description of function seurat2cellexalvr
#' @export seurat2cellexalvr
if ( ! isGeneric('seurat2cellexalvr') ){setGeneric('seurat2cellexalvr', ## Name
	function (seuratObj) { 
		standardGeneric('seurat2cellexalvr') 
	}
) }

setMethod('seurat2cellexalvr', signature = c ('seurat'),
	definition = function (seuratObj) {

    cell.meta <- data.frame(Identity=as.vector(seuratObj@ident))

    if (exists("Phase", where = seuratObj@meta.data) == T) {
        cell.meta$Phase <- as.vector(seuratObj@meta.data$Phase)
    }

    cell.meta.10 <- make.cell.meta.from.df(cell.meta,colnames(cell.meta))
    rownames(cell.meta.10) <- seuratObj@cell.names
    
    cellObj <- methods::new("cellexalvr", data = as.matrix(seuratObj@data), meta.cell = as.matrix(cell.meta.10))

    if (exists("pca", where = seuratObj@dr) == T) {
        pca <- as.matrix(seuratObj@dr$pca@cell.embeddings[,1:3])
        cellObj <- addDRC2cellexalvr(cellObj,pca,"PCA")
    }

    if (exists("tsne", where = seuratObj@dr) == T) {
        tsne <- as.matrix(seuratObj@dr$tsne@cell.embeddings)
        if(ncol(tsne)<3){
            stop("Number of compoments is less than 3. Rerun \"RunTSNE\" using \"dim.embed=3\" to make use of all that VR goodness")
        }else{
            cellObj <- addDRC2cellexalvr(cellObj,tsne[,1:3],"tSNE")
        }
    }

    cellObj
} )


#' addDRC2cellexalvr is a simple helper function that applies some tests 
#' of usabilty to a 3D DRC matrix object and adds it to the cellexalvrR object.
#' 
#' @name addDRC2cellexalvr
#' @aliases addDRC2cellexalvr,cellexalvrR-method
#' @rdname addDRC2cellexalvr-methods
#' @docType methods
#' @description  Adds drc coordinates to a 'cellexalvrObj'
#' @param cellexalObj, cellexalvr object
#' @param drcmatrix A (3 columns) matrix of coordinates
#' @param name A name for the object (default = graph<n>)
#' @title description of function 'addDRC2cellexalvr'
#' @export addDRC2cellexalvr
if ( ! isGeneric('addDRC2cellexalvr') ){setGeneric('addDRC2cellexalvr', ## Name
    function (cellexalObj, drcmatrix, name=NULL) { 
        standardGeneric('addDRC2cellexalvr')
    }
) }

setMethod('addDRC2cellexalvr', signature = c ('cellexalvrR'),
    definition = function (cellexalObj, drcmatrix, name=NULL) {

    rq.ind <- (length(cellexalObj@drc)+1)
    if ( ! is.null(name) ){
        rq.nm <- name
    }else {
        rq.nm <- paste("graph",(length(cellexalObj@drc)+1),sep="")
    }
    mp <- drcmatrix
    colnames(mp) <- c("x","y","z")
    rownames(mp) <- colnames(cellexalObj@data)

    cellexalObj@drc[[rq.ind]] <- mp
    names(cellexalObj@drc)[rq.ind] <- rq.nm
    cellexalObj
} )


#' @name changeIdent
#' @aliases changeIdent,seurat-method
#' @rdname changeIdent-methods
#' @docType methods
#' @description  Sets new cell indentities from a given list
#' @param seuratObj A cellexalvr object
#' @param cell A cellexalvr object
#' @param new.idents  TEXT MISSING
#' @keywords cell type
#' @title description of function changeIdent
#' @export changeIdent
if ( ! isGeneric('changeIdent') ){setGeneric('changeIdent', ## Name
	function (seuratObj,new.idents) { 
		standardGeneric('changeIdent') 
	}
) }

setMethod('changeIdent', signature = c ('seurat'),
	definition = function (seuratObj,new.idents) {

    new.ids <- rep("",length(seuratObj@cell.names))

    for(i in 1:length(new.idents)){

        new.ids[grep(as.character(-i),seuratObj@cell.names)] <- new.idents[i]

    }
    seuratObj<- Seurat::SetIdent(seuratObj,ident.use=new.ids)
    seuratObj
} )
