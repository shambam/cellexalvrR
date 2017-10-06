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