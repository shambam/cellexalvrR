context('VR stop logging' )

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

prefix = './'

script = file.path(prefix, 'data/vrscripts/logStop.R')

datadir <- file.path(prefix, 'data/output/default_user' ) ## please give me the user spcific analysis path here!!!!

ofiles <- c( 'testSession/_bookdown.yml', 'session-log-for-session-testsession.html', 'search_index.json' )

for ( fname in ofiles ) {
	if (  file.exists( file.path(datadir,  fname ) ) ){
		file.remove( file.path(datadir,  fname ) )
	}	
}

system( paste( 'Rscript', script, datadir  ) )

test <- loadObject(file.path(datadir, "cellexalObj.RData"))

expect_true( is.null(test@usedObj$sessionName) , "session has ended" )


for ( fname in ofiles ){
	expect_true( file.exists( file.path(datadir, fname ) ) , paste( "file has not been created",  file.path(datadir,fname) ))
}
