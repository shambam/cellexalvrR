context('create sessionPath')

prefix = './'
#prefix = 'tests/testthat'

data = file.path(prefix, 'data/cellexalObj.RData')

cellexalObj = loadObject( data )
cellexalObj@userGroups=data.frame()
cellexalObj@usedObj$lastGroup = NULL
cellexalObj@usedObj$SelectionFiles = list()
datadir <- normalizePath(file.path( prefix, 'data', 'output'))
cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL
cellexalObj@outpath = file.path(datadir) ## to not mess up the package

pidfile = file.path(cellexalObj@outpath, 'mainServer.pid')

if ( file.exists(pidfile)){
	unlink( pidfile )
}

cellexalObj = sessionPath( cellexalObj )

#"2020_09_30_09_17_08"
seped = as.numeric(unlist(stringr::str_split (cellexalObj@usedObj$sessionName,"_")))
expect_true( length(seped) == 6)
expect_true( all( is.numeric(seped)))

expect_true( file.exists(cellexalObj@usedObj$sessionPath ), label="session path created")

for ( f in c('png', 'tables') ) {
	expect_true( file.exists(file.path(cellexalObj@usedObj$sessionPath,f) ), label=paste("session sub-path",f) )

}

## this should not be overwritable without a renderReport!
old= cellexalObj@usedObj$sessionName
cellexalObj = sessionPath( cellexalObj, 'somethingNew' )
expect_true( cellexalObj@usedObj$sessionName == old, label="session name is not changable in session")

old= cellexalObj@usedObj$sessionName
cellexalObj = sessionPath( cellexalObj, 'somethingNew' )
expect_true( cellexalObj@usedObj$sessionName == old, label="session name is really not changable in session")

context('create sessionPath - simulated server')

cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL


cat( Sys.getpid() , file = pidfile )

cellexalObj = sessionPath( cellexalObj, old )

expect_true(file.exists( file.path(cellexalObj@outpath, 'mainServer.sessionName')), label='file mainServer.sessionName')

cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL

cellexalObj = sessionPath( cellexalObj, 'something' )

expect_true(cellexalObj@usedObj$sessionName ==  old, label=paste("session name is read from file old =",old, "== new =", cellexalObj@usedObj$sessionName," ?") )


unlink( pidfile )
unlink( file.path(cellexalObj@outpath, 'mainServer.sessionName') )

