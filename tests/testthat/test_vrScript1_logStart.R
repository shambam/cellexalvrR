prefix = ''

script = file.path(prefix, 'data/vrscripts/logStart.R')

datadir <- file.path(prefix, 'data/output/default_user' ) ## please give me the user spcific analysis path here!!!!

sessionString <- 'testSession'

if ( file.exists( file.path(datadir,'testSession' ) )){
	unlink ( file.path(datadir,'testSession' ), recursive = TRUE )
}

system( paste( 'Rscript', script, datadir, sessionString  ) )

cellexalObj <- loadObject(file.path(datadir, "cellexalObj.RData"))

expect_true(cellexalObj@usedObj$sessionName == 'testSession' )
expect_true(cellexalObj@usedObj$sessionName == file.path(datadir,'testSession') )

opaths <- c( 'testSession', 'testSession/png','testSession/tables' )

for ( fname in opaths){
	expect_true( file.exists( file.path(datadir,  fname ) ) , paste( "path exists", fname) )
}

