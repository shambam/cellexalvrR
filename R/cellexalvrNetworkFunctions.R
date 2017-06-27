#'Creates a network from selected groups for selected genes
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param location of TF file
#'@param outfile The name of the output file
#'@keywords network construction
#'@export make.cellexalvr.network

make.cellexalvr.network <- function(cellexalObj,cellidfile,tf.loc,outfile){

    dat <- cellexalObj@data

    cellid <- read.delim(cellidfile,header=F)
    
    grp.vec <- as.vector(cellid[,2])
    grps <- as.vector(unique(cellid[,2]))
    
    req.graph <- unique(cellid[,3])
    
    tfs <- as.vector(read.delim(tf.loc)[,4])

    tfs.in.d <- intersect(tfs[!tfs==""],rownames(dat))

    grp.tabs <- NULL

    for(i in 1:length(grps)){

        sub.d <- dat[match(tfs.in.d,rownames(dat)),as.vector(cellid[which(cellid[,2]==grps[i]),1])]

        inferred.pcor <- ggm.estimate.pcor(t(sub.d),method="static")
        test.results <- network.test.edges(inferred.pcor,plot=F)
        net <- extract.network(test.results, cutoff.ggm=0.9)
        net[,2] <- rownames(sub.d)[net[,2]]
        net[,3] <- rownames(sub.d)[net[,3]]

        key <- paste(net[,2],net[,3],sep="")

        grp.tabs <- rbind(grp.tabs,cbind(net,grps[i],key))
    }
    grp.tabs   

}

