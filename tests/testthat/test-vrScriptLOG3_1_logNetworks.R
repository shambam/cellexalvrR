skip('deprecated')

context('VR log network 1')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}
prefix = './'

script = file.path(prefix, 'data/vrscripts/logNetwork.R')

datadir <- file.path(prefix, 'data/output/default_user' ) ## please give me the user spcific analysis path here!!!!

if ( ! file.exists(datadir ) ) {
	dir.create(datadir )
	dir.create(file.path( datadir, 'tmp') )
}
png( file=file.path(datadir, 'tmp', 'a_simple_figure2.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()

heatmap_png <- file.path(datadir, 'tmp', 'a_simple_figure2.png')

grouping <- file.path(prefix, 'data/selection0.txt')

## NO genes, ontology and no topNodes this time.

ofiles = c( file.path('png', 'a_simple_figure2.png'), 'png/User.group.1.graph1.2_3.png', 'png/User.group.1.graph1.1_2.png'  )


for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}

system( paste( 'Rscript', script, datadir, heatmap_png, grouping  ) )

for ( fname in ofiles){
	expect_true( file.exists( file.path(datadir, 'testSession',  fname ) ) , paste( "file has not been created", fname) )
}


