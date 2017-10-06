#' @name export2cellexalvr
#' @aliases export2cellexalvr,cellexalvr-method
#' @rdname export2cellexalvr-methods
#' @docType methods
#' @description Creates all files necessary for the VR application.
#' @param forceDB re-write the db even if it exisis (default =F)
#' @param cellexalObj the cellexalvr object to be exported
#' @param path the path to create the files in
#' @param forceDB re-create the expression data sglite database (default=FALSE)
#' @title description of function export2cellexalvr
#' @export 
setGeneric('export2cellexalvr', ## Name
	function (cellexalObj,path, forceDB=F) { ## Argumente der generischen Funktion
		standardGeneric('export2cellexalvr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('export2cellexalvr', signature = c ('cellexalvr'),
	definition = function (cellexalObj,path, forceDB=F) {

	
	ofile = file.path( path, "cellexalObj.RData")
	if ( ! file.exists( ofile) ){
		save(cellexalObj,file=ofile )
	}
    

    #write.table(cellexalObj$data,paste(path,"expression.expr",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	ofile = file.path(path,"a.meta.cell")
	if ( ! file.exists( ofile) ){
		write.table(cellexalObj$meta.cell,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}
	ofile = file.path(path,"index.facs")
	if ( ! file.exists( ofile) ){
		write.table(cellexalObj$index,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}
	ofile = file.path(path,"c.meta.gene")
	if ( ! file.exists( ofile) ){
		write.table(cellexalObj$meta.gene,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}

    for(i in 1:length(cellexalObj$mds)){
        
        #ashape <- ashape3d(as.matrix(cellexalObj$mds[[i]]), alpha = 5)
		#ofile = file.path(path,paste("graph",i,".hull",sep=""))
		ofile = file.path(path,paste(names(cellexalObj$mds)[i],".hull",sep=""))
		if ( ! file.exists( ofile )) {
        	rq.tring <- NULL

        	if(entropy(as.matrix(cellexalObj$mds[[i]]))<0){
	            ashape <- ashape3d(as.matrix(cellexalObj$mds[[i]]), alpha = 5)
            	#rgl.open()
            	#plot(ashape)
            	rq.triang <- ashape$triang[which(ashape$triang[,9]>1),1:3]
        	}

        	if(entropy(as.matrix(cellexalObj$mds[[i]]))>0){
	            ashape <- ashape3d(as.matrix(cellexalObj$mds[[i]]), alpha = 2)
            	#rgl.open()
            	#plot(ashape)
            	rq.triang <- ashape$triang[which(ashape$triang[,9]>1),1:3]
        	}
			write.table(rq.triang,ofile,row.names=T,col.names=F,quote=F,sep="\t",eol="\n")
		}
		ofile = file.path(path,paste(names(cellexalObj$mds)[i],".mds",sep=""))
		if ( ! file.exists( ofile )) {
			write.table(cellexalObj$mds[[i]],ofile,row.names=T,col.names=F,quote=F,sep="\t",eol="\n")
		}
    }
	
	ofile = file.path(path,"database.sqlite")
	if ( file.exists( ofile ) ) {
		if ( forceDB ){
			unlink( ofile ) ## always create the database?!
		}
	}
	if ( ! file.exists( ofile ) ) {
	    genes <- tolower(rownames(cellexalObj$data))
		genes <- data.frame( 'id' = 1:length(genes), genes= genes )
	
		cells <- data.frame( 'id'= 1:ncol(cellexalObj$data), sample= colnames(cellexalObj$data) )
	
	    cdat <- data.frame(genes=genes$id,cellexalObj$data)
	
		colnames(cdat) <- c( 'genes', cells$id )
	
	    md <- melt(cdat, id=('genes') )
	
    	mdc <- md[-which(md[,3]==0),]

		colnames(mdc) <- c('gene_id', 'cell_id','value')
		mdc$cell_id <- as.numeric(as.character(mdc$cell_id))
		colnames(genes) <- c('id', 'gname')
		colnames(cells) <- c('id','cname')
	
    	con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
		
    	RSQLite::dbWriteTable(con, "datavalues",mdc)
		RSQLite::dbWriteTable(con, "genes", genes)
		RSQLite::dbWriteTable(con, "cells", cells )
		
		dbGetQuery(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
		
    	RSQLite::dbDisconnect(con)
	}
	invisible(cellexalObj)

} )
