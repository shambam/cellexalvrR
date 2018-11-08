context('VR init session')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

prefix <- './'

script =file.path( prefix, 'data', 'vrscripts', 'initial_check.R' )

dataSourceFolder <- file.path( prefix, 'data')

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

OV <- loadObject(file.path(dataSourceFolder, "cellexalObj.RData"))

expect_true( all.equal( names(OV@mds), c('graph1', 'graph2')), paste("input object mds names == ('graph1', 'graph2') [", paste(collapse=", ", names(OV@mds)) ))


OV2 <- loadObject(file.path(outputFolder, "cellexalObj.RData"))

#print( paste("Script tests file content:" , file.path(outputFolder, "cellexalObj.RData") ))

expect_true( all.equal( names(OV2@mds), c('graph1', 'graph2')), paste("output object mds names == ('graph1', 'graph2') [", paste(collapse=", ", names(OV2@mds)) ))

