context('server Simple - screenshots')

prefix= './'
#prefix = 'tests/testthat'

path = file.path( prefix,'data', 'output','serverAsFunction')

if ( file.exists( path)){
	unlink( path, recursive=TRUE)
}

dir.create( path )

cellexalObj@outpath = path

setTimeLimit(2,2)
tryCatch({
server ( file=file.path(cellexalObj@outpath,'mainServer'), debug=TRUE, asFunction=TRUE)
}, error= function(err) {"error is planned and can be ignored here" })
setTimeLimit(Inf,Inf)

files = c(  "mainServer.cellexalvrR.version", "mainServer.pid", "mainServer.sessionName" )

for ( f in files ){
	expect_true( file.exists( file.path( cellexalObj@outpath, f) ), label=f ) 
}

