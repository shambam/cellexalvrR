#' Converts a seurat class to one of cellexalvr. If the cell-cycle score have been calculated they will be added to the cell.mata table
#' @param seuratObj the suerat object to be converted
#' @export seurat2cellexalvr

seurat2cellexalvr <- function(seuratObj) 
{

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
}

#'Runs DDRtree for a Seurat class
#'@param seuratObj A cellexalvr object
#'@keywords ddrtree
#'@export run.ddrtree

run.ddrtree <- function(seuratObj){

    dat.samp <- as.matrix(seuratObj@data[seuratObj@var.genes,])
    ddr.samp <- DDRTree((dat.samp), dimensions=3)
    ddr.cood <- t(ddr.samp$Z)
    ddr.cood    
}

#'Sets new cell indentities from a given list
#'@param seuratObj A cellexalvr object
#'@param cell A cellexalvr object
#'@keywords cell type
#'@export changeIdent


changeIdent <- function(seuratObj,new.idents){

    new.ids <- rep("",length(seuratObj@cell.names))

    for(i in 1:length(new.idents)){

        new.ids[grep(as.character(-i),seuratObj@cell.names)] <- new.idents[i]

    }
    seuratObj<- SetIdent(seuratObj,ident.use=new.ids)
    seuratObj
}