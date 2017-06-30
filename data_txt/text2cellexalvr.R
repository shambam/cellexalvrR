libraray(cellexalvr)
meta.cell <- read.delim('Cell_Types.txt')
index <- read.delim('Surface_marker_expression.txt')
dat <- read.delim('Gene_Expression_Data.txt')
DDTree <- read.delim('Expression_data_3D_projection_DDRtree.txt')
TSNE <- read.delim('Expression_data_3D_projection_TSNE.txt')

dat <- dat[-which(dat[,1] == ""),]
rownames(dat) <- dat[,1]
meta.gene <- data.frame('GeneID' = dat[,1] )
dat <- dat[,-1]

obj <- cellexalvr$new(dat, meta.gene =meta.gene, meta.cell=meta.cell  )
obj$mds$cell$DDRTree <- DDTree
obj$mds$cell$TSNE <- TSNE

obj$index <- index

save( obj, file='../data/obj.RData')

