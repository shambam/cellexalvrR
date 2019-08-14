skip('deprecated')

context('VR log start')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}
prefix = './'


script = file.path(prefix, 'data/vrscripts/logStart.R')

datadir <- file.path(prefix, 'data/output/default_user' ) ## please give me the user spcific analysis path here!!!!

sessionString <- 'testSession'

if ( file.exists( file.path(datadir,'testSession' ) )){
	unlink ( file.path(datadir,'testSession' ), recursive = TRUE )
}

system( paste( 'Rscript', script, datadir, sessionString  ) )

cellexalObj <- loadObject(file.path(datadir, "cellexalObj.RData"))

expect_true(cellexalObj@usedObj$sessionName == 'testSession' )
expect_true( cellexalObj@usedObj$sessionPath == normalizePath(file.path(datadir,'testSession')) )

opaths <- c( 'testSession', 'testSession/png','testSession/tables',  'testSession/00.SessionStart.Rmd')

for ( fname in opaths){
	expect_true( file.exists( file.path(datadir,  fname ) ) , paste( "file has not been created", fname) )
}

