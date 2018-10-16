prefix = './'

script = file.path(prefix, 'data/vrscripts/GOanalysis.R')


datadir <- file.path(prefix, 'data/output/default_user' ) ## please give me the user spcific analysis path here!!!!

genes <- file.path(prefix, 'data/heatmap_0.txt') ## the heatmap_<x>.txt file

if ( ! file.exists( file.path(datadir, 'cellexalObj.RData' ))) {
	stop("please run the logStart first!")
}

## NO genes, ontology and no topNodes this time.

ofiles = c(file.path('tables', '1.GOanalysis.csv'), file.path('tables', '1.GOgenes.csv'))


for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}

system( paste( 'Rscript', script, datadir, genes ) )

for ( fname in ofiles){
	expect_true( file.exists( file.path(datadir, 'testSession',  fname ) ) , paste( "file has not been created", fname) )
}


