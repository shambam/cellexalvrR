context('create session grouping')

prefix = './'

data = file.path(prefix, 'data/cellexalObj.RData')

cellexalObj = loadObject( data )

cellexalObj@outpath = file.path(prefix, 'data', 'output') ## to not mess up the package

## init a session
lockedSave(cellexalObj)
cellexalObj = sessionPath( cellexalObj, 'sessionGroupingTest' )

expect_true( file.exists( file.path(prefix, 'data', 'output', 'sessionGroupingTest' ) ) , "session path has not been created" )

grouping =  file.path(prefix, 'data', 'selection0.txt' )

cellexalObj = userGrouping( cellexalObj, grouping )
cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 1, paste("first entry not 1(", n, ")"))


grouping =  file.path(prefix, 'data', 'selection1.txt' )

cellexalObj = userGrouping( cellexalObj, grouping )
cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 2, paste("second entry not 2(", n, ")"))

grouping =  file.path(prefix, 'data', 'selection0.txt' )
cellexalObj = userGrouping( cellexalObj, grouping )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 1, paste("third try: first entry not 1(", n, ")"))

## try to render a session - that might take forever!

ofile=  file.path( prefix, 'data', 'output', 'session-log-for-session-sessiongroupingtest.html')
if( file.exists(ofile)) {
	unlink(ofile)
}

renderReport ( cellexalObj )

expect_true(file.exists( ofile), 'html report / padoc installed?')