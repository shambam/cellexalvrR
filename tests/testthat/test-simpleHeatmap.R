context('function test simpleHeatmaps function')

prefix = '.'

#print(file.path(getwd(), prefix, 'data', 'simpleHeatmap_mat.RData' ) )
load( file.path(prefix, 'data', 'simpleHeatmap_mat.RData' ) )

load( file.path(prefix, 'data', 'simpleHeatmap_time.RData' ) )

## now I have a mat object

opath = file.path(prefix, 'data', 'output','simpleHeatmap' )

if ( ! file.exists(opath) ){
	dir.create( opath, recursive=TRUE)
}

fname= file.path(opath, 'simpleHeatmap' )

x = methods::new('cellexalvrR')

x@usedObj$timelines[[basename(fname)]] = time

res = simplePlotHeatmaps (x, mat, fname )

load( file.path(prefix, 'data','simpleHeatmap_res.RData' ) )

expect_equal( res, exp )

