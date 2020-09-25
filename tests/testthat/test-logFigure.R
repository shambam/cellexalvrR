context('log figure integrated')

prefix = './'

# prefix ='tests/testthat'

## need to start from scratch - why not a little bit heatmap first?

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj
x@outpath = file.path(prefix,'data','output','LogFigure' )

if ( ! file.exists(x@outpath)){
	dir.create( x@outpath )
}else {
	unlink(  x@outpath, recursive=T )
	dir.create( x@outpath )
}

grouping <- file.path(prefix, 'data/selection0.txt')

x = sessionPath( x, 'logFigureTest')

context('log figure - stats first')

x@userGroups=data.frame()
x@usedObj$lastGroup = NULL
x@usedObj$SelectionFiles = list()
x = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1 )

expect_true( length( x@usedObj$deg.genes) == 102, info = paste("wrong gene number c++ wilcox", length( gene2) ) )
x= logStatResult ( x, x@usedObj$sigGeneLists$Cpp[[x@usedObj$lastGroup]],method= 'wilcox', 'p.value')

ofile=  file.path( x@usedObj$sessionPath, 'AB_Stats_paritalLog.Rmd' )
expect_true( file.exists( ofile), label = ofile)


context('log figure - figure next')

png( file= file.path( x@usedObj$sessionPath, 'testScreenshot.png') ,width=800, height=800)
plot(1:10,10:1, main="Not reall a screenshot :-D")
dev.off()

x= logFigure( x, png=file.path( x@usedObj$sessionPath, 'testScreenshot.png'), text="Not really a Screenshot, but a test file...") 


ofile=  file.path( x@usedObj$sessionPath, 'AC_Figure_paritalLog.Rmd' )
expect_true( file.exists( ofile), label = ofile)


## And the push a heatmap after that - this does not work in VR - hence thsi test!

context('log figure - heatmap last')

png( file= file.path( x@usedObj$sessionPath, 'testHeatmap.png') ,width=800, height=800)
plot(10:1,1:10, main="Not reall a heatmap :-D")
dev.off()


cellexalObj = logHeatmap(x, x@usedObj$deg.genes, 
	file.path( x@usedObj$sessionPath, 'testHeatmap.png'), grouping )

ofile=  file.path( x@usedObj$sessionPath, 'AD_Heatmap_paritalLog.Rmd' )
expect_true( file.exists( ofile), label = ofile)

