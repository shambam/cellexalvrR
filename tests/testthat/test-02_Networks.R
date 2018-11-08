context('Networks')
opath = file.path('data','output')

ofiles =c( 'Networks.nwk', 'NwkCentroids.cnt' )

for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	if(  file.exists(ofile ) ){
		unlink( ofile)
	}
}
cellexalObj = loadObject( file.path(opath,'..', 'output', 'cellexalObj.RData') )
cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs')




make.cellexalvr.network ( file.path(opath,'..', 'output', 'cellexalObj.RData') , file.path(opath,'..', 'selection0.txt'), opath )

for ( f in ofiles ) {
	ofile = file.path(opath, f )
	print( ofile )
	expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
}



