#'Creates the base files needed to run the VR environment
#'@param cellexalObj A cellexalvr object
#'@export export2cellexalvr

export2cellexalvr <- function(cellexalObj,path){

    save(cellexalObj,file=paste(path,"cellexalObj.RData",sep=""))

    write.table(cellexalObj@data,paste(path,"expression.expr",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n")
    write.table(cellexalObj@meta.cell,paste(path,"a.meta.cell",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n") 
    write.table(cellexalObj@index,paste(path,"index.facs",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n")
    write.table(cellexalObj@meta.gene,paste(path,"c.meta.gene",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n")

    for(i in 1:length(cellexalObj@mds)){
        write.table(cellexalObj@mds[[i]],paste(path,"graph",i,".mds",sep=""),row.names=T,col.names=F,quote=F,sep="\t",eol="\r\n")
    }


    genes <- rownames(cellvr@data)

    cdat <- data.frame(genes=genes,cellvr@data)
    md <- melt(cdat)

    con <- dbConnect(SQLite(),dbname = "database.sqlite")
    dbWriteTable(con, "data",md)
    dbDisconnect(con)
}


#'Makes the base files needed to run the VR environment from a Seurat object
#'@param cellexalObj A cellexalvr object
#'@export seurat2cellexalvr


seurat2cellexalvr <- function(seuratObj){

    cell.att <- as.vector(seuratObj@ident)
    cell.t <- unique(cell.att)

    cell.met <- matrix(0,ncol=length(cell.t),nrow=length(cell.att))

    for(i in 1:length(cell.t)){
        ind <- which(cell.att==cell.t[i])
        cell.met[ind,i] <- 1
    }

    colnames(cell.met) <- paste(cell.t,".type",sep="")
    rownames(cell.met) <- seuratObj@cell.names
    proj <- seuratObj@tsne.rot
    colnames(proj) <- c("x","y","z")

    g <- new("cellexalvr",data=as.matrix(seuratObj@data),mds=list(graph1=proj),meta.cell=as.matrix(cell.met))
    #export2cellexalvr(g,path)
}

#'Adds mds coordinates to a cellexalvrObj
#'@param cellexalObj A cellexalvr object
#'@param mdsmatrix A matrix of coordinates
#'@export addMDS2cellexalvr

addMDS2cellexalvr <- function(cellexalObj,mdsmatrix){

    rq.ind <- (length(cellexalObj@mds)+1)
    rq.nm <- paste("graph",(length(cellexalObj@mds)+1),sep="")
    mp <- mdsmatrix
    colnames(mp) <- c("x","y","z")
    rownames(mp) <- colnames(cellexalObj@data)

    cellexalObj@mds[[rq.ind]] <- mp
    names(cellexalObj@mds)[rq.ind] <- rq.nm
    cellexalObj
}