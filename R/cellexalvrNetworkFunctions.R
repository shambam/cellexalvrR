#'Creates a network from selected groups for selected genes
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param location of TF file
#'@param outfile The name of the output file
#'@keywords network construction
#'@export make.cellexalvr.network

make.cellexalvr.network <- function(cellexalObjpath,cellidfile,outpath){

    #dat <- cellexalObj@data

    load(cellexalObjpath)

    dat <- cellexalObj@data
    cellid <- read.delim(cellidfile,header=F)
    
    grp.vec <- as.vector(cellid[,2])
    grps <- as.vector(unique(cellid[,2]))
    
    req.graph <- unique(cellid[,3])
    
    #tfs <- as.vector(read.delim(tf.loc)[,4])

    tfs.in.d <- intersect(cellexalObj@tfs[!cellexalObj@tfs==""],rownames(dat))

    grp.tabs <- NULL
    avg.mds.coods <- NULL

    for(i in 1:length(grps)){

        rq.cells <- as.vector(cellid[which(cellid[,2]==grps[i]),1])

        sub.d <- dat[match(tfs.in.d,rownames(dat)),rq.cells ]

        inferred.pcor <- ggm.estimate.pcor(t(sub.d),method="static")
        test.results <- network.test.edges(inferred.pcor,plot=F)
        net <- extract.network(test.results, cutoff.ggm=0.8)
        net[,2] <- rownames(sub.d)[net[,2]]
        net[,3] <- rownames(sub.d)[net[,3]]

        key1 <- paste(net[,2],net[,3],sep="")
        key2 <- paste(net[,3],net[,2],sep="")

        grp.tabs <- rbind(grp.tabs,cbind(net,grps[i],key1,key2))

        #make avg coods
        
        avg.mds.coods <- rbind(avg.mds.coods, c(apply(cellexalObj@mds[[req.graph]][rq.cells,],2,mean),grps[i]))


    }
    
    write.table(grp.tabs,paste(outpath,"Networks.nwk",sep=""),row.names=F,col.names=T,quote=F,sep="\t")
    write.table(cbind(avg.mds.coods,req.graph),paste(outpath,"NwkCentroids.cnt",sep=""),row.names=F,col.names=F,quote=F,sep="\t")

}
