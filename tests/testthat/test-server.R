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


write_lines <- function( x, f=scriptfile) {
	max = 5
	while ( file.exists(scriptfile) ){
		Sys.sleep( 1 )
		max = max -1;
		if(max == 0)
			break;
	}
	file.create(lockfile)
	
	fileConn<-file(f)
	writeLines( x , fileConn)
	close( fileConn )
	file.remove(lockfile)
	
	invisible(NULL)
}
	
write_lines( c( 
	"library(cellexalvrR)", 
	paste(sep="","server( file='",tmpFile,"')" ) ), 
	f= srvFile 
)



system( paste( file.path(R.home("bin"), "R CMD BATCH") , srvFile , " &") )

Sys.sleep(3)

file.create(scriptfile)
expect_true(file.exists( scriptfile))

Sys.sleep( 2 )

expect_true(!file.exists( scriptfile))

expect_true(file.exists( pidfile ))
pid = scan( pidfile )

## would only work on linux....
system(paste( sep="",'ps -af | grep "',pid,'"  | grep -v grep '))

write_lines( c(
paste(sep="", "png(file='",tmpFile,".png', width=800, height=800)"),
"plot ( 1:10, 1:10)",
"dev.off()"))

while( file.exists(scriptfile )){
	Sys.sleep( 1 )
}

expect_true(file.exists( paste(tmpFile, 'png', sep='.' )))

write_lines(  paste(sep="", "This will bvbreake horribly") )

## so now lets try some real things
## we have a inbuild dataset ;-)
## Hence we can do funny things like get a list of differential genes
write_lines( paste( "cellexalObj@outpath='",tmpFile,"'", sep="") )
dir.create( tmpFile )

Sys.sleep( 5 )

system(paste( sep="",'ps -af | grep "',pid,'"  | grep -v grep '))

write_lines(c(" ", paste( sep="",
	"make.cellexalvr.heatmap.list(cellexalObj, 'User.group.2', 250, '",paste(sep=".", tmpFile, "diffGenes"),"', 'wilcox' )" 
)	) )

Sys.sleep( 5 )

expect_true(!file.exists( scriptfile))

expect_true(file.exists( paste(tmpFile, 'diffGenes', sep='.' )))
diffGenes = scan( paste(tmpFile, 'diffGenes', sep='.' ), what=character())[-1]

expect_true( length(diffGenes) == 250)

## that does not work - the networks need different input?
write_lines( paste( sep="",
	"make.cellexalvr.network(cellexalObj, 'User.group.2', '",paste(sep=".", tmpFile, "Networks"),"', 'wilcox' )" 
)	)
## but the block in the write_lines works fine!

write_lines( paste( sep="", "get.genes.cor.to(cellexalObj, '",  diffGenes[3], "', output = '",  paste(tmpFile, 'corrGenes', sep='.' ) ,"',  is.smarker= FALSE, cpp=TRUE)" ))
Sys.sleep( 5 )

expect_true(!file.exists( scriptfile))

expect_true (file.exists( paste(tmpFile, 'corrGenes', sep='.' ) ) )

unlink( pidfile ) ## shut the server down

Sys.sleep( 3 )


file.create(scriptfile)
expect_true(file.exists( scriptfile))

Sys.sleep( 2 )

expect_true(file.exists( scriptfile)) ## server is down

file.remove( scriptfile )

## more later..
