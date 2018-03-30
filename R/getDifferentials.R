#'Creates a heatmap from a selection of groups
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param deg.method The method to use to find DEGs
#'@param numsig The number of differentials to be returned
#'@keywords DEGs
#'@export getDifferentials

getDifferentials <- function(cellexalObj,cellidfile,deg.method=c("anova","DESeq"),num.sig){

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

    col.tab <- info$col

    if(length(rem.ind)>0){
		dat.f <- dat.f[-rem.ind,]
	}
	
    deg.genes <- NULL

    if(deg.method=="anova"){

        anovap <- function(v,labs){
		    anova(lm(v~-1+labs))$Pr[1]
	    }

        if ( length(col.tab) > 1 ){
		    ps <- apply(dat.f,1,anovap,labs=grp.vec)
	    }else if (length(col.tab) == 1 ){
		    ps <- apply(dat.f,1,lin,order=1:ncol(dat.f))
	    }
	
	    sigp <- order(ps)[1:num.sig]
	
	    deg.genes <- rownames(dat.f[sigp,])
    }
    
	if(deg.method=="DESeq"){

		user.sel <- data.frame(selection=labs)

		dso <- DESeqDataSetFromMatrix(countData = data.f,colData = user.sel,design = ~ condition)
		dso <- DESeq(dso, test="LRT", reduced=~1)
		res <- results(dso)
		deg.genes<- res
	}


    deg.genes
}