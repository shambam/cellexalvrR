context('server basic testing')

tmpFile = tempfile(pattern = "file", tmpdir = tempdir(), fileext = "")

srvFile =  paste( tmpFile, 'serverR', sep='.')

cat( "library(cellexalvrR)", file= srvFile )
cat( paste(sep="","server( file='",tmpFile,"')" ), file= srvFile )

system( paste( file.path(R.home("bin"), "Rscript") , srvFile ) )



pidfile    = paste( tmpFile, 'pid', sep='.')
scriptfile= paste( sep=".", tmpFile, 'input.R')
lockfile   = paste( tmpFile, 'input.lock', sep=".") 

expect_true(file.exists( pidfile ))
pid = scan( pidfile )



file.create(lockfile)
cat( paste(sep="", "png(file='",tempfile,".png', width=800, height=800)"), file= scriptfile )
cat( "print ( 1:10, 1:10)", file= scriptfile )
cat( "dev.off()", file= scriptfile )
file.remove(lockfile)

while( file.exists(scriptfile )){
	Sys.sleep( 1 )
}

expect_true(file.exist( paste(tmpFile, 'png', sep='.' )))

file.create(lockfile)
cat( paste(sep="", "This will bvbreake horribly"), file= scriptfile )
file.remove(lockfile)


