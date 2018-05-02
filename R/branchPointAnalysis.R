#'Performs a branch-point analysis identifying genes that have opposite changes in two branches stemming from one trunk in a pseudotime analysis
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param maxsig The number of differentials to be returned
#'@param outfile The name of the output file
#'@keywords branch point analysis
#'@export branch.point.analysis

branch.point.analysis <- function(cellexalObj,cellidfile,maxsig,outfile){

    cellexalObj <- loadObject(cellexalObj)
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

    print(grp.vec)

    grp.un <- unique(grp.vec)

    wilcox.test.vec <- function(v,ind1,ind2){
        wilcox.test(v[ind2],v[ind2])$p.value
    }

    branch.1 <- apply(dat,1,wilcox.test.vec,ind1=which(grp.vec==grp.un[1]),ind2=which(grp.vec==grp.un[3]))
    branch.1
}