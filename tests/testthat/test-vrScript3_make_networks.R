skip('deprecated')

context('VR create network')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}
prefix = './'

script = file.path(prefix, 'data/vrscripts/make_networks.R')


input_file <- file.path(prefix, 'data/output/default_user', 'User.group.1.cgr'  ) # grouping file path

datadir <-  file.path(prefix, 'data/output/default_user' ) # the user specific folder

output_file  <- file.path(prefix, 'data/output/default_user', "Resources","Networks" ) # the output path

ofiles = c( 'Networks.nwk', 'NwkCentroids.cnt' )

for ( fname in ofiles ) {
	if (  file.exists( file.path(output_file, fname ) ) ){
		file.remove( file.path(output_file, fname ) )
	}	
}

CO <- loadObject( file.path(datadir, 'cellexalObj.RData' ) )

expect_true( all.equal( names(CO@drc), c('graph1', 'graph2')), paste("before: input object drc names == ('graph1', 'graph2') [", 
				paste(collapse=", ", names(CO@drc)) ))


system( paste( 'Rscript', script, input_file, datadir, output_file ))


CO <- loadObject( file.path(datadir, 'cellexalObj.RData' ) )

expect_true( all.equal( names(CO@drc), c('graph1', 'graph2')), paste("after: input object drc names == ('graph1', 'graph2') [", 
				paste(collapse=", ", names(CO@drc)), "]" ))



for ( fname in ofiles ) {
	expect_true( file.exists( file.path(output_file, fname ) ), paste("file has not been created", fname) )
}