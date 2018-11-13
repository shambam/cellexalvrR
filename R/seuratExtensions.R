#' @name seurat2cellexalvr
#' @aliases seurat2cellexalvr,seurat-method
#' @rdname seurat2cellexalvr-methods
#' @docType methods
#' @description  Converts a seurat class to one of cellexalvr. If the cell-cycle score have been calculated
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
    
    cellObj <- new("cellexalvr", data = as.matrix(seuratObj@data), meta.cell = as.matrix(cell.meta.10))

    if (exists("pca", where = seuratObj@dr) == T) {
        pca <- as.matrix(seuratObj@dr$pca@cell.embeddings[,1:3])
        cellObj <- addMDS2cellexalvr(cellObj,pca,"PCA")
    }

    if (exists("tsne", where = seuratObj@dr) == T) {
        tsne <- as.matrix(seuratObj@dr$tsne@cell.embeddings)
        if(ncol(tsne)<3){
            stop("Number of compoments is less than 3. Rerun \"RunTSNE\" using \"dim.embed=3\" to make use of all that VR goodness")
        }else{
            cellObj <- addMDS2cellexalvr(cellObj,tsne[,1:3],"tSNE")
        }
    }

    cellObj
} )
#' @name run.ddrtree
#' @aliases run.ddrtree,seurat-method
#' @rdname run.ddrtree-methods
#' @docType methods
#' @description  Runs DDRtree for a Seurat class
#' @param seuratObj A cellexalvr object
#' @title description of function run.ddrtree
#' @keywords ddrtree
#' @export run.ddrtree
if ( ! isGeneric('run.ddrtree') ){setGeneric('run.ddrtree', ## Name
	function (seuratObj) { 
		standardGeneric('run.ddrtree') 
	}
) }

setMethod('run.ddrtree', signature = c ('seurat'),
	definition = function (seuratObj) {

    dat.samp <- as.matrix(seuratObj@data[seuratObj@var.genes,])
    ddr.samp <- DDRTree::DDRTree((dat.samp), dimensions=3)
    ddr.cood <- t(ddr.samp$Z)
    ddr.cood    
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
