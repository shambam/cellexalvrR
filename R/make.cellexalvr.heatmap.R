#' @name make.cellexalvr.heatmap
#' @aliases make.cellexalvr.heatmap,cellexalvr-method
#' @rdname make.cellexalvr.heatmap-methods
#' @docType methods
#' @description A VR function that creates a heatmap
#' @param cvrObj the cellexavr object or the file to load the object from
#' @param cellidfile the grouping or a file to read a grouping from
#' @param num.sig the amount of significant genes to plot
#' @param outfile the file to plot to (png)
#' @title description of function make.cellexalvr.heatmap
#' @export 
if ( ! isGeneric('make.cellexalvr.heatmap') ){ setGeneric('make.cellexalvr.heatmap', ## Name
		function (cvrObj,cellidfile,num.sig,outfile) { ## Argumente der generischen Funktion
			standardGeneric('make.cellexalvr.heatmap') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)
}else {
	print ("Onload warn generic function 'make.cellexalvr.heatmap' already defined - no overloading here!")
}
setMethod('make.cellexalvr.heatmap', signature = c ('character'),
		definition = function (cvrObj,cellidfile,num.sig,outfile) {
			make.cellexalvr.heatmap( loadObject(cvrObj),cellidfile,num.sig,outfile) 
		}
)


setMethod('make.cellexalvr.heatmap', signature = c ('cellexalvr'),
		definition = function (cvrObj,cellidfile,num.sig,outfile) {
			
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
			not <- which(is.na(cellexalObj$userGroups[,cellexalObj$usedObj$lastGroup]))
			if ( length(not) > 0) {
				loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj$data)[- not ] )
			}else {
				loc <- cellexalObj
			}
			loc <- reorder.samples ( loc, paste(cellexalObj$usedObj$lastGroup, 'order'))
			
			info <- groupingInfo( loc )
			
			dat <- loc$data
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
)
