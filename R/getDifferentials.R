#'Creates a heatmap from a selection of groups
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param deg.method The method to use to find DEGs
#'@param numsig The number of differentials to be returned
#'@keywords DEGs
#'@export getDifferentials

getDifferentials <- function(cellexalObj,cellidfile,deg.method=c("anova","DESeq"),numsig){

    cellexalObj <- loadObject(cellexalObj)

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

    rem.ind <- which(apply(dat,1,sum)==0)
	dat.f <- dat

	grp.vec <- info$grouping

    if(length(rem.ind)>0){
		dat.f <- dat.f[-rem.ind,]
	}
	
    deg.genes <- NULL

    if(deg.method=="anova"){

        if ( length(col.tab) > 1 ){
		    ps <- apply(dat.f,1,anovap,labs=grp.vec)
	    }else if (length(col.tab) == 1 ){
		    ps <- apply(dat.f,1,lin,order=1:ncol(dat.f))
	    }
	
	    sigp <- order(ps)[1:num.sig]
	
	    deg.genes <- rownames(dat.f[sigp,])
    }
    
    deg.genes
}