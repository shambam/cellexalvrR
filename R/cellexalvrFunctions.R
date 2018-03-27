
#'Creates a heatmap from a selection of groups
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param numsig The number of differentials to be returned
#'@param outfile The name of the output file
#'@keywords heatmap
#'@export make.cellexalvr.heatmap

make.cellexalvr.heatmap <- function(cvrObj,cellidfile,num.sig,outfile){
	
	anovap <- function(v,labs){
		anova(lm(v~-1+labs))$Pr[1]
	}
	
	lin <- function( v, order ) {
		cor.test( v, order, method='spearman')$p.value
	}
	
	cellexalObj <- loadObject(cvrObj)
	## now I want to store the grouping in the cellexalvr object
#	browser()
	
	cellexalObj <- userGrouping(cellexalObj, cellidfile)
	not <- which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
	if ( length(not) > 0) {
		loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj@data)[- not ] )
	}else {
		loc <- cellexalObj
	}

	loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order'))
	info <- groupingInfo( loc )

	dat <- loc@data
	#cellid <- read.delim(cellidfile,header=F)
	
	grp.vec <- info$grouping
	
	col.tab <- info$col
	
	for(i in 1:length(col.tab)){
		ind <- which(grp.vec==col.tab[i])
		grp.vec[ind] <- paste("Grp",i,sep="")
	}
	rcolrs <- list(Group=col.tab)
	names(rcolrs$Group) <- unique(grp.vec)
	
	rem.ind <- which(apply(dat,1,sum)==0)
	dat.f <- dat
	
	if(length(rem.ind)>0){
		dat.f <- dat.f[-rem.ind,]
	}
	
	if ( length(col.tab) > 1 ){
		ps <- apply(dat.f,1,anovap,labs=grp.vec)
	}else if (length(col.tab) == 1 ){
		ps <- apply(dat.f,1,lin,order=1:ncol(dat.f))
	}
	
	sigp <- order(ps)[1:num.sig]
	
	annotation_col = data.frame(Group = (grp.vec))
	rownames(annotation_col) <- colnames(dat.f)
	
	png(outfile,height=2000,width=2000)

	pheatmap(dat.f[sigp,],cluster_rows=TRUE, show_rownames=T,show_colnames=FALSE,cluster_cols=FALSE,
			scale="row",clustering_method = "ward.D2",col=bluered(16),breaks=seq(-4,4,by=0.5),
			annotation_col = annotation_col,annotation_colors=rcolrs
	)
	dev.off()
	invisible(cellexalObj)
}

#'Creates a list of genes from which a heatmap is made in Unity
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param numsig The number of differentials to be returned
#'@param outfile The name of the output file
#'@keywords heatmap
#'@export make.cellexalvr.heatmap.list

make.cellexalvr.heatmap.list <- function(cvrObj,cellidfile,num.sig,outfile){
	
	anovap <- function(v,labs){
		anova(lm(v~-1+labs))$Pr[1]
	}
	
	lin <- function( v, order ) {
		cor.test( v, order, method='spearman')$p.value
	}
	
	cellexalObj <- loadObject(cvrObj)
	## now I want to store the grouping in the cellexalvr object
#	browser()
	
	cellexalObj <- userGrouping(cellexalObj, cellidfile)
	not <- which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
	if ( length(not) > 0) {
		loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj@data)[- not ] )
	}else {
		loc <- cellexalObj
	}

	loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order'))
	info <- groupingInfo( loc )

	dat <- loc@data
	#cellid <- read.delim(cellidfile,header=F)
	
	grp.vec <- info$grouping
	
	col.tab <- info$col
	
	for(i in 1:length(col.tab)){
		ind <- which(grp.vec==col.tab[i])
		grp.vec[ind] <- paste("Grp",i,sep="")
	}
	rcolrs <- list(Group=col.tab)
	names(rcolrs$Group) <- unique(grp.vec)
	
	rem.ind <- which(apply(dat,1,sum)==0)
	dat.f <- dat
	
	if(length(rem.ind)>0){
		dat.f <- dat.f[-rem.ind,]
	}
	
	if ( length(col.tab) > 1 ){
		ps <- apply(dat.f,1,anovap,labs=grp.vec)
	}else if (length(col.tab) == 1 ){
		ps <- apply(dat.f,1,lin,order=1:ncol(dat.f))
	}
	
	sigp <- order(ps)[1:num.sig]
	
	reduced.matrix <- dat.f[sigp,]

	gene.cluster.order <- rownames(reduced.matrix)[hclust(as.dist(1-cor(t(reduced.matrix))))$order]
	
	write(c(num.sig,gene.cluster.order),file=outfile,ncolumns=1)

	#annotation_col = data.frame(Group = (grp.vec))
	#rownames(annotation_col) <- colnames(dat.f)
	
	#png(outfile,height=2000,width=2000)

	#pheatmap(dat.f[sigp,],cluster_rows=TRUE, show_rownames=T,show_colnames=FALSE,cluster_cols=FALSE,
	#		scale="row",clustering_method = "ward.D2",col=bluered(16),breaks=seq(-4,4,by=0.5),
	#		annotation_col = annotation_col,annotation_colors=rcolrs
	#)
	#dev.off()
	#invisible(cellexalObj)
}



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
	
	cellexalObj
}

#'Gets positively and negatively correlated genes to a chosen gene
#'@param cellexalObj A cellexalvr object
#'@param gname The required gene
#'@param is.smarker Whether the supplied gene is a surface marker
#'@keywords correlation
#'@export get.genes.cor.to
get.genes.cor.to <- function(cellexalObj, gname, output, is.smarker=F){
	
	cellexalObj <- loadObject(cellexalObj)
	dat <- cellexalObj@data
	rownames(dat) <- tolower(rownames(dat))
	
	goi <- NULL

	if(is.smarker==F){
		goi <- dat[gname,]
	}
	
	if(is.smarker==T){
		m <- match( to.lower(gname), to.lower(colnames(cellexalObj@index)))
		goi <- cellexalObj@index[,m]
	}
	
	if ( is.null(goi)) {
		stop( paste( gname, "neither a gene nor a fascs maker name") )
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