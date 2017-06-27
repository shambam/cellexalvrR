#'Creates a network from selected groups for selected genes
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param numsig The number of differentials to be returned
#'@param outfile The name of the output file
#'@keywords heatmap
#'@export make.cellexalvr.network

make.cellexalvr.network <- function(cellexalObj,cellidfile,tf.loc){

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

        grp.tabs <- rbind(grp.tabs,cbind(net,grps[i]))
    }
    grp.tabs   

}


load("gedata.RData")
dat <- as.matrix(gedata[,-1])
rownames(dat) <- gedata[,1]


tfs <- as.vector(read.delim("/home/shamit/Annotation/TFs/Mouse/Mouse_TFs_v2.txt",header=F)[,4])

selc <- read.delim("selection0.txt")

grps <- as.vector(unique(selc[,2]))

tfs.in.d <- intersect(tfs[!tfs==""],rownames(dat))

grp.tabs <- NULL

##

for(i in 1:length(grps)){

    sub.d <- dat[match(tfs.in.d,rownames(dat)),as.vector(selc[which(selc[,2]==grps[i]),1])]

    inferred.pcor <- ggm.estimate.pcor(t(sub.d),method="static")
    test.results <- network.test.edges(inferred.pcor,plot=F)
    net <- extract.network(test.results, cutoff.ggm=0.9)
    #gr2 <- network.make.graph(net, rownames(sub.d), drop.singles=TRUE)
    #gr2.ig <- as.undirected(graph_from_graphnel(gr2))
    #l <- layout_with_kk(gr2.ig)

    #x11()
    #plot(gr2.ig,vertex.size=0)
    net[,2] <- rownames(sub.d)[net[,2]]
    net[,3] <- rownames(sub.d)[net[,3]]

    grp.tabs <- rbind(grp.tabs,cbind(net,grps[i]))
}
