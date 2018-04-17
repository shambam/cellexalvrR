#'Loads the cellexalvr object, if the fname is a file
#'@param fname the file to load or a cellexalvr object
#'@keywords load
#'@export loadObject
loadObject <- function( fname, maxwait=50 ) {
	if ( class(fname)[1] == 'cellexalvr' ) {
		cellexalObj = fname
	}else {
		if ( file.exists( fname) ) {
			waited = 0
			while ( file.exists( paste(fname, 'lock',sep='.'))){
				Sys.sleep(1)
				waited = waited +1
				if ( waited == maxwait) { break }
			}
			if (waited != maxwait ){
				load(fname)
			}else {
				stop( paste("Could not obtain access to locked file", fname ))
			}
		}
	}
	cellexalObj
}


#'Loads TF annotation into cellexalvr object
#'@param cellexalObj A cellexalvr object
#'@param specie The specie required
#'@keywords TFs
#'@export set.specie

set.specie <- function(cellexalObj,specie=c("mouse","human")){
	
	if(specie=="mouse"){
		data(mouse.tfs)
		cellexalObj@tfs <- mouse.tfs
	}
	
	if(specie=="human"){
		data(human.tfs)
		cellexalObj@tfs <- human.tfs
	}

	cellexalObj@specie <- specie
	cellexalObj
}

#'Gets positively and negatively correlated genes to a chosen gene
#'@param cellexalObj A cellexalvr object
#'@param gname The required gene
#'@param output the outfile
#'@param is.smarker Whether the supplied gene is a surface marker (default =F)
#'@keywords correlation
#'@export get.genes.cor.to
get.genes.cor.to <- function(cellexalObj, gname, output, is.smarker=F){
	
	cellexalObj <- loadObject(cellexalObj)
	dat <- cellexalObj@data
	rownames(dat) <- tolower(rownames(dat))
	
	goi <- NULL

	if(is.smarker==F){
		goi <- dat[tolower(gname),]
	}
	
	if(is.smarker==T){
		m <- match( tolower(gname), tolower(colnames(cellexalObj@index)))
		

		if(is.na(m)==T)
			stop("Gene name is not an available surface marker")

		goi <- cellexalObj@index[,m]
	}
	
	if ( is.null(goi)) {
		stop( paste("the gname", gname, "is neither a gene nor a fascs maker name", paste( colnames(cellexalObj@index), collapse=", ")) )
	}
	
	calc.cor <- function(v, comp){
		cor(v, comp)
	}
	
	cor.values <- apply(dat,1,calc.cor,comp=goi)
	
	ord <- names(sort(cor.values))
	
	pos <- ord[ (length(ord)-1): (length(ord)-10) ]
	neg <- ord[1:10]
	tab <- cbind(pos,neg)
	
	write.table(t(tab),output,row.names=F,col.names=F,sep="\t",quote=F)
}

