
script ='data/vrscripts/initial_check.R'

dataSourceFolder <- 'data'

outputFolder <- 'data/output/default_user'

system( paste( 'Rscript', script, dataSourceFolder, outputFolder ))


for ( fname in c( 'cellexalObj.RData', 'groupings_info.txt', 'User.group.1.cgr', 'User.group.2.cgr' ) ){
	
	expect_true( file.exists( file.path(outputFolder, fname ) ) , paste( "file exists", fname) )
}
