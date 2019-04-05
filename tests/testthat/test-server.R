context('server basic testing')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

tmpFile = tempfile(pattern = "file", tmpdir = tempdir(), fileext = "")

srvFile =  paste( tmpFile, 'serverR', sep='.')

pidfile    = paste( tmpFile, 'pid', sep='.')
scriptfile= paste( sep=".", tmpFile, 'input.R')
lockfile   = paste( tmpFile, 'input.lock', sep=".") 


## This function is crucial as it checks that the R script is finished using file.exists(scriptfile)
## and waits. For a working version should it also restart the server after some time?
write_lines <- function( x, f=scriptfile, maxWait=5) {
	while ( file.exists(f) ){
		Sys.sleep( 1 )
		maxWait = maxWait -1;
		if(maxWait == 0)
			break;
	}
	if ( file.exists(f) ){
		stop( "Server is not clear for usage - start a new instance?")
	}
	file.create(lockfile)
	
	fileConn<-file(f)
	writeLines( x , fileConn)
	close( fileConn )
	file.remove(lockfile)
	
	invisible(NULL)
}

wait4server<- function( file=scriptfile,  maxWait=15) {
	while ( file.exists(file) ){
		Sys.sleep( 1 )
		maxWait = maxWait -1;
		if(maxWait == 0)
			break;
	}
	if ( file.exists(file) ){
		stop( "Server is not clear for usage - start a new instance?")
	}
}
	
write_lines( c( 
	"library(cellexalvrR)", 
	paste(sep="","server( file='",tmpFile,"')" ) ), 
	f= srvFile 
)



system( paste( file.path(R.home("bin"), "R CMD BATCH") , srvFile , " &") )

file.create(scriptfile)
expect_true(file.exists( scriptfile))

wait4server ( )

expect_true(!file.exists( scriptfile))

expect_true(file.exists( pidfile ))
pid = scan( pidfile )

## would only work on linux....
#system(paste( sep="",'ps -af | grep "',pid,'"  | grep -v grep '))

context('server create a png')

write_lines( c(
paste(sep="", "png(file='",tmpFile,".png', width=800, height=800)"),
"plot ( 1:10, 1:10)",
"dev.off()"))

while( file.exists(scriptfile )){
	Sys.sleep( 1 )
}

expect_true(file.exists( paste(tmpFile, 'png', sep='.' )))

context('server does not crash using useless blaberish')

write_lines(  paste(sep="", "This will bvbreake horribly") )

write_lines(  paste(sep="", "stop('This will bvbreake horribly')") )


context('server make.cellexalvr.heatmap.list')


## so now lets try some real things
## we have a inbuild dataset ;-)
## Hence we can do funny things like get a list of differential genes
write_lines( paste( "cellexalObj@outpath='",tmpFile,"'", sep="") )
dir.create( tmpFile )

#system(paste( sep="",'ps -af | grep "',pid,'"  | grep -v grep '))

write_lines(c(" ", paste( sep="",
	"make.cellexalvr.heatmap.list(cellexalObj, 'User.group.2', 250, '",paste(sep=".", tmpFile, "diffGenes"),"', 'wilcox' )" 
)	) )


wait4server()
expect_true(!file.exists( scriptfile))

expect_true(file.exists( paste(tmpFile, 'diffGenes', sep='.' )))
diffGenes = scan( paste(tmpFile, 'diffGenes', sep='.' ), what=character())[-1]

expect_true( length(diffGenes) == 250)


context('server make.cellexalvr.network')

## that does not work - the networks need different input?
write_lines( paste( sep="",
	"make.cellexalvr.network(cellexalObj, 'User.group.2', '",paste(sep=".", tmpFile, "Networks"),"', 'wilcox' )" 
)	)
## but the block in the write_lines works fine!

context('server get.genes.cor.to')

write_lines( paste( sep="", "get.genes.cor.to(cellexalObj, '",  diffGenes[3], "', output = '",  paste(tmpFile, 'corrGenes', sep='.' ) ,"',  is.smarker= FALSE, cpp=TRUE)" ))

wait4server()

expect_true(!file.exists( scriptfile))

expect_true (file.exists( paste(tmpFile, 'corrGenes', sep='.' ) ) )


## more later..


context('server shutdown when pid file is lost')

unlink( pidfile ) ## shut the server down

Sys.sleep( 3 )

file.create(scriptfile)
expect_true(file.exists( scriptfile)) ## no stange error?
Sys.sleep( 2 )

expect_true(file.exists( scriptfile)) ## server is down

file.remove( scriptfile )

