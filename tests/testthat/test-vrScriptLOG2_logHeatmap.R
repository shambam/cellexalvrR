context('VR log heatmap 1')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}
prefix = './'

script = file.path(prefix, 'data/vrscripts/logHeatmap.R')

datadir = normalizePath(file.path(prefix, 'data/output/default_user' ) )# <user specific folder>

genes <- file.path(prefix, 'data/heatmap_0.txt') ## the heatmap_<x>.txt file


if ( ! file.exists(file.path(datadir, 'tmp')) ){
	dir.create(file.path(datadir, 'tmp'), recursive = TRUE )
}

png( file=file.path(datadir, 'tmp', 'a_simple_figure.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()


heatmap_png <- file.path(datadir, 'tmp', 'a_simple_figure.png')

grouping <- file.path(prefix, 'data/selection0.txt')

ontology <- 'BP'

topNodes  <- 20

ofiles = c( 'png/User.group.1.graph1.1_2.png', 'png/a_simple_figure.png','png/User.group.1.graph1.2_3.png' )

for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}

system( paste( 'Rscript', script, datadir, genes, heatmap_png, grouping, ontology, topNodes ))

for ( fname in c( ofiles, '00.SessionStart.Rmd') ){
	expect_true( file.exists( file.path(datadir, 'testSession',  fname ) ) , paste( "file has not been created",file.path(datadir, 'testSession', fname) ))
}


