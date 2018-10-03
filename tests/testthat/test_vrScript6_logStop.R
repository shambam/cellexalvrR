prefix = ''

script = file.path(prefix, 'data/vrscripts/logStop.R')

datadir <- file.path(prefix, 'data/output/default_user' ) ## please give me the user spcific analysis path here!!!!

ofiles <- c( '_bookdown.yml', 'testSession.html', 'search_index.json' ,'knit.R' )

for ( fname in ofiles ) {
	if (  file.exists( file.path(datadir, 'testSession', fname ) ) ){
		file.remove( file.path(datadir, 'testSession', fname ) )
	}	
}

system( paste( 'Rscript', script, datadir  ) )

test <- loadObject(file.path(datadir, "cellexalObj.RData"))

expect_true( is.null(test@usedObj$sessionName) , "session has ended" )


for ( fname in ofiles ){
	expect_true( file.exists( file.path(datadir, 'testSession', fname ) ) , paste( "file exists", fname) )
}
