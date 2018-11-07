context('VR log network 1')

prefix = './'

script = file.path(prefix, 'data/vrscripts/logNetwork.R')


datadir <- file.path(prefix, 'data/output/default_user' ) ## please give me the user spcific analysis path here!!!!

png( file=file.path(datadir, 'tmp', 'a_simple_figure2.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()

heatmap_png <- file.path(datadir, 'tmp', 'a_simple_figure2.png')

grouping <- file.path(prefix, 'data/selection0.txt')

## NO genes, ontology and no topNodes this time.

ofiles = c( file.path('png', 'a_simple_figure2.png') )


for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}

system( paste( 'Rscript', script, datadir, heatmap_png, grouping  ) )

for ( fname in ofiles){
	expect_true( file.exists( file.path(datadir, 'testSession',  fname ) ) , paste( "file has not been created", fname) )
}


