library(cellexalvr)

opath = file.path('data','output')

ofiles =c( 'Networks.nwk', 'NwkLayouts.lay', 'NwkCentroids.cnt' )

for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	if(  file.exists(ofile ) ){
		unlink( ofile)
	}
}

test_that( "Network outfiles are produced" ,{
			
			make.cellexalvr.network ( file.path('data', 'output', 'cellexalObj.RData') , file.path('data', 'selection0.txt'), opath )
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f )
				print( ofile )
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
			
		})