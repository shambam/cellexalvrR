#'Creates the base files needed to run the VR environment
#'@param cellexalObj A cellexalvr object
#' @param path the path for the files
#'@export export2cellexalvr

export2cellexalvr <- function(cellexalObj,path){

    save(cellexalObj,file=paste(path,"cellexalObj.RData",sep=""))

    write.table(cellexalObj$data,paste(path,paste( cellexalObj$name,"expression.data",sep="."),sep=""),row.names=T,col.names=T,quote=F,sep="\t")
    write.table(cellexalObj$meta.cell,paste(path,,paste( cellexalObj$name,"meta.cell",sep="."),sep=""),row.names=T,col.names=T,quote=F,sep="\t") 
    write.table(cellexalObj$index,paste(path,,paste( cellexalObj$name,"index",sep="."),sep=""),row.names=T,col.names=T,quote=F,sep="\t")
    write.table(cellexalObj$meta.gene,paste(path,,paste( cellexalObj$name,"meta.gene",sep="."),sep=""),row.names=T,col.names=T,quote=F,sep="\t")

    for(i in 1:length(cellexalObj$mds)){
        write.table(cellexalObj$mds[[i]],paste(path,"graph",i,".mds",sep=""),row.names=T,col.names=T,quote=F,sep="\t")
    }

}