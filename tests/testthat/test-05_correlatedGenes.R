context('get.genmes.cor.to function')

prefix = './'

genes <- file.path(prefix, 'data/heatmap_0.txt')


genes = read.delim(genes)[,1]

cellexalObj <- loadObject(file.path('data','cellexalObj.RData') )

ofile= 'Procr_correlated_genes.txt' 

cellexalObj@outpath = file.path('data','output','statTest' )

tab = get.genes.cor.to (cellexalObj, gname = 'Procr', output=  ofile, is.smarker = F, cpp=T )

tab1 = get.genes.cor.to (cellexalObj, gname = 'Procr', output=  ofile, is.smarker = F, cpp=F )

expect_equal( tab, tab1, "c++ same outpout as R")

##expect_true( length( gene3) == 100, paste("wrong gene number seurat wilcox", length( gene3) ) )


#test_that( 'MAST' ,{
#	skip_if_not_installed( 'Seurat' )		
#	options(warn=-1)
#	gene1 = getDifferentials(x, grouping, 'MAST', num.sig=100, Log=FALSE )
#	options(warn=1)
#	expect_true( length( gene1) == 100,  paste("wrong gene number MAST", length( gene1) )  )
#})
#
#test_that( 'poisson' ,{
#	skip_if_not_installed( 'Seurat' )
#	gene1 = getDifferentials(x, grouping, 'poisson', num.sig=100 , Log=FALSE)
#	expect_true( length( gene1) == 100, paste("wrong gene number poisson", length( gene1) )  )
#})

#gene1 = getDifferentials(x, grouping, 'anova', num.sig=100 )




## get a grouping added?
