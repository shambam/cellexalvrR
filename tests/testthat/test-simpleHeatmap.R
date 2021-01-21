context('function test simpleHeatmaps function')

#prefix = 'tests/testthat'
prefix = '.'

#print(file.path(getwd(), prefix, 'data', 'simpleHeatmap_mat.RData' ) )

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x = reset(x)

x@outpath = file.path(prefix,'data','output','simpleHeatmaps' )
if ( file.exists(x@outpath ) ){
	unlink( x@outpath ,recursive=TRUE)
}
dir.create( x@outpath )

x = sessionPath(x, 'simpleHeatmap' )


grouping <- normalizePath (file.path(prefix, 'data', 'SelectionHSPC_time.txt' ))
x = userGrouping( x, grouping)
x = pseudotimeTest3D( x, grouping = x@usedObj$lastGroup )

x = createStats( x@usedObj$timelines[[1]], x)

timeline = x@usedObj$timelines[[1]]

expect_equal( timeline@gname, 'Time.group.2', "time gname is correct" )


fname= file.path(x@usedObj$sessionPath,'png', 'simpleHeatmap' )

res = simplePlotHeatmaps (x, info = groupingInfo( x, timeline@gname), fname )

expect_equal( names(res), c("genes", "ofile", "pngs", "error", "mat") )
expect_equal( length(res$genes), 6, label="6 gene groups")
expect_equal( res$ofile , paste(sep=".", fname, 'png'), label="ofile correct" )
expect_equal( length(res$pngs), 6, label="6 heatmap pngs")

for ( f in c( res$ofile, (res$pngs )) ){
	expect_true( file.exists( f ), label=f )
}

expect_equal( res$error, NULL, label="no error" )

expect_equal( dim(res$mat), c( 159, 250 ), label="zscored dimension OK" )


