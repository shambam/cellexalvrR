#' @name make.cellexalvr.heatmap.list
#' @aliases make.cellexalvr.heatmap.list,cellexalvrR-method
#' @rdname make.cellexalvr.heatmap.list-methods
#' @docType methods
#' @description  Creates an outfile for CellexalVR containing a list of genes from which a heatmap is made in VR
#' Internally this function uses the getDifferentials() function and just writes the output of that into the outfile.
#' @param cvrObj A cellexalvr object
#' @param cellidfile file containing cell IDs or the grouping name
#' @param num.sig The number of differentials to be returned
#' @param outfile The name of the output file
#' @param stats_method the stats method to use see getDifferentials()
#' @title Create the gene list files for VR
#' @keywords differential, gene list
#' @export make.cellexalvr.heatmap.list
if ( ! isGeneric('make.cellexalvr.heatmap.list') ){setGeneric('make.cellexalvr.heatmap.list', ## Name
			function (cvrObj,cellidfile,num.sig,outfile, stats_method=NA ) { 
				standardGeneric('make.cellexalvr.heatmap.list') 
			}
	) }

setMethod('make.cellexalvr.heatmap.list', signature = c ('character'),
		definition = function (cvrObj,cellidfile,num.sig,outfile, stats_method=NA ) {
			cvrObj <- loadObject( cvrObj )
			make.cellexalvr.heatmap.list(cvrObj,cellidfile,num.sig,outfile, stats_method )
		}
)


setMethod('make.cellexalvr.heatmap.list', signature = c ('cellexalvrR'),
		definition = function (cvrObj,cellidfile,num.sig,outfile, stats_method=NA) {
			
			if ( is.na(stats_method) )
				stats_method= 'wilcox'
			
			gene.cluster.order = getDifferentials(cvrObj,cellidfile, stats_method, num.sig= num.sig)
			message (paste( "trying to write file", outfile ) )
			write(c(length(gene.cluster.order),gene.cluster.order),file=outfile,ncolumns=1)
		} )


#' @name make.cellexalvr.heatmap
#' @aliases make.cellexalvr.heatmap,cellexalvrR-method
#' @rdname make.cellexalvr.heatmap-methods
#' @docType methods
#' @description  Creates a heatmap from a selection of groups
#' This function is deprecated
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param numsig The number of differentials to be returned
#' @param outfile The name of the output file
#' @param num.sig  TEXT MISSING
#' @keywords heatmap
#' @title description of function make.cellexalvr.heatmap
#' @export make.cellexalvr.heatmap
if ( ! isGeneric('make.cellexalvr.heatmap') ){setGeneric('make.cellexalvr.heatmap', ## Name
	function (cellexalObj, cellidfile,num.sig,outfile) { 
		standardGeneric('make.cellexalvr.heatmap') 
	}
) }

setMethod('make.cellexalvr.heatmap', signature = c ('character'),
		definition = function (cellexalObj, cellidfile,num.sig,outfile) {
			cellexalObj <- loadObject(cellexalObj)
			make.cellexalvr.heatmap( cellexalObj, cellidfile,num.sig,outfile )
		}
)

setMethod('make.cellexalvr.heatmap', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cellidfile,num.sig,outfile) {
	
		message( "WARNING:: you should not use this functionm any more! (make.cellexalvr.heatmap)")
	anovap <- function(v,labs){
		anova(lm(v~-1+labs))$Pr[1]
	}
	
	lin <- function( v, order ) {
		cor.test( v, order, method='spearman')$p.value
	}
	
	cellexalObj <- loadObject(cellexalObj)
	## now I want to store the grouping in the cellexalvr object
#	browser()
	
	cellexalObj <- userGrouping(cellexalObj, cellidfile)
	not <- which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
	if ( length(not) > 0) {
		loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj@dat)[- not ] )
	}else {
		loc <- cellexalObj
	}

	loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order'))
	info <- groupingInfo( loc )

	dat <- loc@dat
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
	
	pheatmap::pheatmap(dat.f[sigp,],cluster_rows=TRUE, show_rownames=T,show_colnames=FALSE,cluster_cols=FALSE,
			scale="row",clustering_method = "ward.D2",col=bluered(16),breaks=seq(-4,4,by=0.5),
			annotation_col = annotation_col,annotation_colors=rcolrs
	)
	dev.off()
	invisible(cellexalObj)
} )

