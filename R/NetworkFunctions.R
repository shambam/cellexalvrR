#'Creates a network from selected groups for selected genes
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param outfile The name of the output file
#' @param cutoff.ggm The cutoff for the correlation (default = 0.8)
#'@keywords network construction
#'@export make.cellexalvr.network

make.cellexalvr.network <- function(cellexalObj,cellidfile,outpath, cutoff.ggm=0.8,numsig=125){

    #dat <- cellexalObj@data
	cellexalObj <- loadObject(cellexalObj)
	
	cellexalObj <- userGrouping(cellexalObj, cellidfile)

	checkVRfiles( cellexalObj, outpath)
	## cut loc to only include TFs
	if ( is.na( match('TFs', colnames(cellexalObj@meta.gene)))) {
		cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs')
	}
	loc <- onlyGOIs( cellexalObj, 'TFs' )
	
	## kick the not groupoed samples out of the loc object
	loc <- reduceTo (loc, what='col', to=colnames(cellexalObj@data)[-
							which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
			] )
	loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order'))
	
	info <- groupingInfo( loc )
	grps <- as.vector(unique(info$grouping))
	
    dat <- loc@data
    req.graph <- info$mds
    
    grp.tabs <- NULL
    avg.mds.coods <- NULL
    #layout.tabs <- NULL

    for(i in 1:length(grps)){

        print(paste("Making network",i))

        rq.cells <- as.vector(colnames(dat)[which(info$grouping==grps[i])])

        sub.d <- dat[, rq.cells ]
        print(dim(sub.d))

        inferred.pcor <- ggm.estimate.pcor(t(sub.d),method="static")
        test.results <- network.test.edges(inferred.pcor,plot=F)
        net <- extract.network(test.results, cutoff.ggm = cutoff.ggm )

        if(nrow(net)>numsig){
            net <- net[1:numsig,]
        }

        avg.mds.coods <- rbind(avg.mds.coods, c(apply(cellexalObj@mds[[req.graph]][rq.cells,],2,mean),info$col[i]))
        
        if(nrow(net)>0){

            net[,2] <- rownames(sub.d)[net[,2]]
            net[,3] <- rownames(sub.d)[net[,3]]

            key1 <- paste(net[,2],net[,3],sep="")
            key2 <- paste(net[,3],net[,2],sep="")

            grp.tabs <- rbind(grp.tabs,cbind(net,info$col[i],key1,key2))

            #igrp <- graph_from_data_frame(as.data.frame(net[,2:3]), directed = TRUE)
  
            #lay <- round(layout_nicely(igrp),6)
            #rownames(lay) <- names(V(igrp))
            #lay[,1] <- rescale(lay[,1],to=c(-1,1))
            #lay[,2] <- rescale(lay[,2],to=c(-1,1))
            #lay <- cbind(lay,info$col[i])
        
            #layout.tabs <- rbind(layout.tabs,lay)
        }else{next}

    }   
    
    
    if(nrow(grp.tabs)==0){
        stop("There are no networks to see here.")
    }else{
    write.table(grp.tabs,file.path( outpath,"Networks.nwk"),row.names=F,col.names=T,quote=F,sep="\t",eol="\r\n")
    write.table(cbind(avg.mds.coods,req.graph),file.path( outpath,"NwkCentroids.cnt"),row.names=F,col.names=F,quote=F,sep="\t",eol="\r\n")
    #write.table(layout.tabs,file.path(outpath,"NwkLayouts.lay"),row.names=T,col.names=F,quote=F,sep="\t",eol="\r\n")
	invisible(cellexalObj)
    }
}
