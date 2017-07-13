
#'Creates a heatmap from a selection of groups
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@param numsig The number of differentials to be returned
#'@param outfile The name of the output file
#'@keywords heatmap
#'@export make.cellexalvr.heatmap

make.cellexalvr.heatmap <- function(cvrObj,cellidfile,num.sig,outfile){

    anovap <- function(v,labs){
        anova(lm(v~-1+labs))$Pr[1]
    }

  load(cvrObj)
  dat <- cellexalObj@data

  cellid <- read.delim(cellidfile,header=F)
  grp.vec <- as.vector(cellid[,2])
  
  col.tab <- unique(as.vector(cellid[,2]))

  for(i in 1:length(col.tab)){
    ind <- which(grp.vec==col.tab[i])
    grp.vec[ind] <- paste("Grp",i,sep="")
  }
  rcolrs <- list(Group=col.tab)
  names(rcolrs$Group) <- unique(grp.vec)
  

  dat.s <- dat[,as.vector(cellid[,1])]

  rem.ind <- which(apply(dat.s,1,sum)==0)
  dat.f <- dat.s

  if(length(rem.ind)>0){

    dat.f <- dat.s[-rem.ind,]
  }

  ps <- apply(dat.f,1,anovap,labs=grp.vec)

  sigp <- order(ps)[1:num.sig]

  annotation_col = data.frame(Group = (grp.vec))
  rownames(annotation_col) <- cellid[,1]
  
  png(outfile,height=2000,width=2000)
  pheatmap(dat.f[sigp,],cluster_rows=TRUE, show_rownames=T,show_colnames=FALSE,cluster_cols=FALSE,scale="row",clustering_method = "ward.D2",col=bluered(16),breaks=seq(-4,4,by=0.5),annotation_col = annotation_col,annotation_colors=rcolrs)
  dev.off()
}




#'Loads TF annotation into cellexalvr object
#'@param cellexalObj A cellexalvr object
#'@param specie The specie required
#'@keywords TFs
#'@export set.specie

set.specie <- function(cellexalObj,specie=c("mouse","human")){

    if(specie=="mouse"){
        data(mouse.tfs)
        cellexalObj@tfs <- mouse.tfs
    }

    if(specie=="human"){
        data(human.tfs)
        cellexalObj@tfs <- human.tfs
    }

    cellexalObj
}

#'Gets positively and negatively correlated genes to a chosen gene
#'@param cellexalObj A cellexalvr object
#'@param gname The required gene
#'@keywords correlation
#'@export get.genes.cor.to
get.genes.cor.to <- function(cellexalObj,gname,output){

    load(cellexalObj)
    dat <- cellexalObj@data

    goi <- dat[gname,]

    calc.cor <- function(v,comp){
        cor(v,comp)
    }

    cor.values <- apply(dat,1,calc.cor,comp=goi)

    ord <- names(sort(cor.values))
    
    pos <- ord[ (length(ord)-1): (length(ord)-10) ]
    neg <- ord[1:10]
    tab <- cbind(pos,neg)

    write.table(tab,output,row.names=F,col.names=F,sep="\t",quote=F)
}