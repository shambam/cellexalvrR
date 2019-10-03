#' @name cormat2df
#' @aliases cormat2df,cellexalvrR-method
#' @rdname cormat2df-methods
#' @docType methods
#' @description  Converts a pairwise correlation matrix for data.frame
#' @param cors A square matrix of pairwise measures
#' @title description of function cormat2df
#' @keywords network construction
#' @export cormat2df
if ( ! isGeneric('cormat2df') ){setGeneric('cormat2df', ## Name
	function (cors) {
		standardGeneric('cormat2df')
	}
) }

setMethod('cormat2df', signature = c ('matrix'),definition = function (cors) {

    ut <- upper.tri(cors)
    df.out <- data.frame(
        row = rownames(cors)[row(cors)[ut]],
        column = rownames(cors)[col(cors)[ut]],
        cor  = (cors)[ut]
    )
    df.out
})

#' This is the R function that calculates the TF networks shown in the VR environment.
#' 
#' @name make.cellexalvr.network
#' @aliases make.cellexalvr.network,cellexalvrR-method
#' @rdname make.cellexalvr.network-methods
#' @docType methods
#' @description  Creates a network from selected groups for selected genes
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param outpath the outpath
#' @param cutoff.ggm The cutoff for the correlation (default = 0.8)
#' @param top.n.inter get only the n top interations default=130
#' @title description of function make.cellexalvr.network
#' @keywords network construction
#' @export make.cellexalvr.network
if ( ! isGeneric('make.cellexalvr.network') ){setGeneric('make.cellexalvr.network', ## Name
	function (cellexalObj, cellidfile,outpath, cutoff.ggm=0.8,top.n.inter=130,method=c("rho.p","pcor")) {
		standardGeneric('make.cellexalvr.network')
	}
) }

setMethod('make.cellexalvr.network', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cellidfile,outpath, cutoff.ggm=0.8, top.n.inter=130,method=c("rho.p","pcor")) {

		if ( !file.exists(outpath)) {
			dir.create( outpath ,  recursive = TRUE)
		}

	datadir <- cellexalObj@outpath
    #data <- cellexalObj@data
	cellexalObj <- userGrouping(cellexalObj, cellidfile) #function definition in file 'userGrouping.R'

	message( paste( "the cellexal object drc names:", paste( collapse= ", ", names(cellexalObj@drc))))
	
	#checkVRfiles( cellexalObj, datadir)
	## cut loc to only include TFs
	if ( is.na( match('TFs', colnames(cellexalObj@meta.gene)))) {
		cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs') #function definition in file 'useInbuiltGOIlists.R'
	}

	loc <- onlyGOIs( cellexalObj, 'TFs' ) #function definition in file 'onlyGOIs.R'

	## kick the not groupoed samples out of the loc object
	loc <- reduceTo (loc, what='col', to=colnames(cellexalObj@data)[- #function definition in file 'reduceTo.R'
							which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
			] )
	loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order')) #function definition in file 'reorder.obj.R'
	info <- groupingInfo( loc ) #function definition in file 'groupingInfo.R'
	if ( info$drc == 'unknown' || is.null( info$drc) ) {
		## just assume the user selected from graph 1
		## better than breaking
		info$drc = names(loc@drc)[1]
	}
	grps <- as.vector(unique(info$grouping))

    data <- loc@data
    req.graph <- info$drc

    grp.tabs <- NULL
    avg.drc.coods <- NULL
    #layout.tabs <- NULL
    if(method=="rho.p"){

        for(i in 1:length(grps)){

            print(paste("Making network",i))

            rq.cells <- as.vector(colnames(data)[which(info$grouping==grps[i])])

            sub.d <- data[, rq.cells ]

            rem.ind<- which(apply(sub.d,1,sum)==0)
            print(dim(sub.d))
            if (length(rem.ind) > 0) {
                sub.d <- sub.d[-rem.ind,]
            }
            cor.mat <- propr::perb(as.matrix(t(sub.d)))@matrix
            cor.mat.flt <- cormat2df(cor.mat) #function definition in file 'NetworkFunctions.R'
            cor.mat.ord <-  cor.mat.flt[rev(order(cor.mat.flt[,3])),]
            
            cor.cut <- quantile(cor.mat.ord[,3],0.99)

            hi.prop <- length(which(cor.mat.ord[,3] > cor.cut))
            lo.prop <- length(which(cor.mat.ord[,3] < -1*cor.cut))


            hi.num <- round(top.n.inter*(hi.prop/(hi.prop+lo.prop)))
            lo.num <- top.n.inter-hi.num

            cor.mat.req <- cor.mat.ord[ c(1:hi.num, (nrow(cor.mat.ord)-lo.num):nrow(cor.mat.ord)),]

            net <- cbind(cor.mat.req[,c(3,1,2)],0,0,0)
            colnames(net) <- c("pcor","node1","node2","pval","qval","prob")
            if ( is.null(rownames(cellexalObj@drc[[req.graph]])) ) {
                rownames(cellexalObj@drc[[req.graph]]) =colnames( data )
            }
            avg.drc.coods <- rbind(avg.drc.coods, c(apply(cellexalObj@drc[[req.graph]][rq.cells,],2,mean),info$col[i]))

             if(nrow(cor.mat.req)>0){

                key1 <- paste(net[,2],net[,3],sep="")
                key2 <- paste(net[,3],net[,2],sep="")

                grp.tabs <- rbind(grp.tabs,cbind(net,info$col[i],key1,key2))

            }else{next}
        }
    
    }

    if(method=="pcor"){

        for(i in 1:length(grps)){

            print(paste("Making network",i))

            rq.cells <- as.vector(colnames(data)[which(info$grouping==grps[i])])

            sub.d <- data[, rq.cells ]
            print(dim(sub.d))

            inferred.pcor <- GeneNet::ggm.estimate.pcor(as.matrix(Matrix::t(sub.d)),method="static")
            test.results <- GeneNet::network.test.edges(inferred.pcor,plot=F)
            net <- GeneNet::extract.network(test.results, cutoff.ggm = cutoff.ggm )

            if(nrow(net)>top.n.inter){
                net <- net[1:top.n.inter,]
            }

            avg.drc.coods <- rbind(avg.drc.coods, c(apply(cellexalObj@drc[[req.graph]][rq.cells,],2,mean),info$col[i]))

            if(nrow(net)>0){

                net[,2] <- rownames(sub.d)[net[,2]]
                net[,3] <- rownames(sub.d)[net[,3]]

                key1 <- paste(net[,2],net[,3],sep="")
                key2 <- paste(net[,3],net[,2],sep="")

                grp.tabs <- rbind(grp.tabs,cbind(net,info$col[i],key1,key2))

            }else{next}

        }
    }
	message( paste( "the cellexal object drc names:", paste( collapse= ", ", names(cellexalObj@drc))))
	
    if(nrow(grp.tabs)==0){
        message("There are no networks to see here.")
        invisible(cellexalObj)
    }else{
    utils::write.table(grp.tabs,file.path( outpath,"Networks.nwk"),row.names=F,col.names=T,quote=F,sep="\t",eol="\r\n")
    utils::write.table(cbind(avg.drc.coods,req.graph),file.path( outpath,"NwkCentroids.cnt"),row.names=F,col.names=F,quote=F,sep="\t",eol="\r\n")
    #write.table(layout.tabs,file.path(outpath,"NwkLayouts.lay"),row.names=T,col.names=F,quote=F,sep="\t",eol="\r\n")
	invisible(cellexalObj)
    }
} )


#' @describeIn make.cellexalvr.network cellexalvrR
#' @docType methods
#' @description preload the cellexalObj.RData file
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param outpath the outpath
#' @param cutoff.ggm The cutoff for the correlation (default = 0.8)
#' @param top.n.inter get only the n top interations default=125
#' @title description of function make.cellexalvr.network
#' @keywords network construction
#' @export make.cellexalvr.network
setMethod('make.cellexalvr.network', signature = c ('character'),
		definition = function (cellexalObj, cellidfile,outpath, cutoff.ggm=0.8, top.n.inter=125) {
			cellexalObj2 <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			make.cellexalvr.network( cellexalObj2, cellidfile,outpath, cutoff.ggm, top.n.inter) #function definition in file 'NetworkFunctions.R'
		}
)
