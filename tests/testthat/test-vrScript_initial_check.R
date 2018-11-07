context('VR init session')

prefix <- './'

script =file.path( prefix, 'data', 'vrscripts', 'initial_check.R' )

dataSourceFolder <- 'data'

outputFolder <- file.path( prefix, 'data', 'output', 'default_user' )

ofiles = c( 'cellexalObj.RData', 'groupings_info.txt', 'User.group.1.cgr', 'User.group.2.cgr' )

for ( fname in ofiles ){
	
	if( file.exists( file.path(outputFolder, fname ) ) ) {
		file.remove(file.path(outputFolder, fname ))
	}
}

system( paste( 'Rscript', script, dataSourceFolder, outputFolder ))


for ( fname in ofiles ){
	
	expect_true( file.exists( file.path(outputFolder, fname ) ) , paste( "file exists", fname) )
}
