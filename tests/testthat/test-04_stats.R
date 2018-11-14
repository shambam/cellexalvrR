context('stat functions')

prefix = './'

genes <- file.path(prefix, 'data/heatmap_0.txt')

genes = read.delim(genes)[,1]

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

gene1 = getDifferentials(x, grouping, 'anova', num.sig=100 )

expect_true( length( gene1) == 100, "wrong gene number anova" )

test_that( 'edgeR' ,{
	skip_if_not_installed( 'edgeR' )
	options(warn=-1)
	gene1 = getDifferentials(x, grouping, 'edgeR', num.sig=100 )
	options(warn=1)
	expect_true( length( gene1) == 100, "wrong gene number anova" )
})


test_that( 'MAST' ,{
	skip_if_not_installed( 'MAST' )
	options(warn=-1)
	gene1 = getDifferentials(x, grouping, 'MAST', num.sig=100 )
	options(warn=1)
	expect_true( length( gene1) == 100, "wrong gene number anova" )
})

test_that( 'Seurat' ,{
	skip_if_not_installed( 'Seurat' )
	gene1 = getDifferentials(x, grouping, 'Seurat', num.sig=100 )
	expect_true( length( gene1) == 100, "wrong gene number anova" )
})

#gene1 = getDifferentials(x, grouping, 'anova', num.sig=100 )




## get a grouping added?
