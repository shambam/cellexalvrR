context('Networks')
prefix = '.'
opath = file.path(prefix, 'data','output')

ofiles =c( 'Networks.nwk', 'NwkCentroids.cnt' )

for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	if(  file.exists(ofile ) ){
		unlink( ofile)
	}
}
cellexalObj = loadObject( file.path(opath, 'cellexalObj.RData') )
cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs')

#browser()
make.cellexalvr.network ( file.path(opath, 'cellexalObj.RData') , file.path(opath,'..', 'selection0.txt'), opath )

for ( f in ofiles ) {
	ofile = file.path(opath, f )

	print( ofile )
	expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
}

## now add the log network test hereprefix = './'

## the network image is created by the VR process - hence I need a dummy here!

genes = rownames(cellexalObj@data)[1:210]
if ( ! file.exists(file.path(opath, 'tmp') )){
	dir.create( file.path(opath, 'tmp') )
}
png( file=file.path(opath, 'tmp', 'a_simple_figure2.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()

heatmap_png <- file.path(opath, 'tmp', 'a_simple_figure2.png')

cellexalObj = sessionPath (cellexalObj, 'LogNetworkTest' )

ofile = file.path( cellexalObj@outpath, '2_Network_LogNetworkTest.html')

if ( file.exists( ofile ) ) {
	unlink( ofile )
}
cellexalObj = logNetwork(cellexalObj, genes, heatmap_png, file.path(opath,'..', 'selection0.txt') )

expect_true( file.exists( ofile ), paste("outfile exists", ofile) )