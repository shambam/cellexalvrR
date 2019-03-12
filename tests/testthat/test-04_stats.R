context('stat functions')

prefix = './'

genes <- file.path(prefix, 'data/heatmap_0.txt')

genes = read.delim(genes)[,1]

cellexalObj <- loadObject(file.path('data','cellexalObj.RData') )

x = reduceTo(cellexalObj, what='row', to=genes )
x@outpath = file.path('data','output','statTest' )

grouping <- file.path(prefix, 'data/selection0.txt')

x = sessionPath( x, 'StatTest' )

ofiles= c( file.path('png', 'hist.User.grouping.1.anova.png'), file.path('tables', 'User.grouping.1.anova.csv' ) )

datadir = x@usedObj$sessionPath

for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}

gene1 = getDifferentials(x, grouping, 'anova', num.sig=50, Log=FALSE, logfc.threshold = .1, minPct=0.1 )

expect_true( length( gene1) == 50, paste("wrong gene number anova", length( gene1) ) )

gene2 = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1 )

expect_true( length( gene2) == 100, paste("wrong gene number c++ wilcox", length( gene2) ) )

gene3 = getDifferentials(x, grouping, 'Seurat_wilcox', num.sig=100, logfc.threshold = .1, minPct=0.1 )

expect_true( length( gene3) == 100, paste("wrong gene number seurat wilcox", length( gene3) ) )


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
