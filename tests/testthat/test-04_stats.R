context('stat functions')

prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = read.delim(genes)[,1]

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','statTest' )
if ( ! file.exists(x@outpath)){
	dir.create(x@outpath)
}

grouping <- file.path(prefix, 'data/selection0.txt')

x = sessionPath( x, 'StatTest' )

ofiles= c( file.path('png', 'hist.User.grouping.1.anova.png'), file.path('tables', 'User.grouping.1.anova.csv' ) )

datadir = x@usedObj$sessionPath

for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}

## outdated!!
#gene1 = getDifferentials(x, grouping, 'anova', num.sig=50, Log=FALSE, logfc.threshold = .1, minPct=0.1 )
#expect_true( length( gene1@usedObj$deg.genes) == 51, info = paste("wrong gene number anova", length( gene1) ) )
x@userGroups=data.frame()
x@usedObj$lastGroup = NULL
x@usedObj$SelectionFiles = list()

gene2 = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1 )

expect_true( length( gene2@usedObj$deg.genes) == 102, info = paste("wrong gene number c++ wilcox", length( gene2) ) )
logStatResult ( gene2, gene2@usedObj$sigGeneLists$Cpp[[gene2@usedObj$lastGroup]],
method= 'wilcox', 'p.value')
ofile=  file.path( x@usedObj$sessionPath, 'AB_Stats_paritalLog.Rmd' )
expect_true( file.exists( ofile), label = ofile)
ofile=  file.path( x@usedObj$outpath, 'AB_Stats_StatTest.html' )



skip("10x speed improvement to Seurats wilcox test implementation")
gene3 = getDifferentials(x, grouping, 'Seurat_wilcox', num.sig=100, logfc.threshold = .1, minPct=0.1 )

expect_true( length( gene3@usedObj$deg.genes) == 101, info = paste("wrong gene number seurat wilcox", length( gene3) ) )


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
