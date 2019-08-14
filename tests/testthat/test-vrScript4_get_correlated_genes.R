skip('deprecated')

context('VR get correlated genes')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}
prefix = './'

prefix = normalizePath(prefix)

script = file.path(prefix, 'data/vrscripts/get_correlated_genes.R')


datadir <-  file.path(prefix, 'data/output/default_user' ) # the user specific folder

outputfile  <- file.path(prefix, 'data/output/default_user', "ProcR_correlated_genes" ) # the output path

gene_name = 'Procr'

facsTypeArg <- FALSE


if (  file.exists( outputfile ) ){
	file.remove( outputfile )
}	


CO <- loadObject( file.path(datadir, 'cellexalObj.RData' ) )

expect_true( all.equal( names(CO@drc), c('graph1', 'graph2')), paste("before: input object drc names == ('graph1', 'graph2') [", 
				paste(collapse=", ", names(CO@drc)) ))

print( paste( 'Rscript', script, datadir, gene_name, outputfile, facsTypeArg ) )

system( paste( 'Rscript', script, datadir, gene_name, outputfile, facsTypeArg ))

expect_true( file.exists( outputfile ), paste("file has not been created", outputfile ) )
