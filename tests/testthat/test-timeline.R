context('timeline')

#skip('not stable enough')

prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = read.delim(genes)[,1]

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@outpath = file.path(prefix,'data','output','timeLineTest' )

x@usedObj$sessionPath = x@usedObj$sessionRmdFiles = x@usedObj$sessionName = NULL

ofile = file.path( prefix, 'data','output','timeLineTest', 'timeSession', '1_OneGroupTime_paritalLog.Rmd' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}
x = sessionPath( x, 'timeSession')

expect_true( x@usedObj$sessionName == 'timeSession', 'session path not set correctly')
gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )


## I need the 3D vectors for the cells in e.g. group 1
x@groupSelectedFrom = list()
x@userGroups = data.frame()
x@usedObj$lastGroup = NULL

x = userGrouping( x, grouping)
#which( x@userGroups[,x@usedObj$lastGroup] == 1 )
#x@userGroups[,'User.group.2'] = NA
#x@userGroups[,'User.group.2 order'] = x@userGroups[,'User.group.1 order']
#x@userGroups[which(x@userGroups[,'User.group.1'] == 2), 'User.group.2'] = 1
#x@usedObj$SelectionFiles[['User.group.2']] = x@usedObj$SelectionFiles[['User.group.1']]

#x@groupSelectedFrom[['User.group.2']] = x@groupSelectedFrom[['User.group.1']] 

#x@groupSelectedFrom[['User.group.2']][['order']] = x@userGroups[,'User.group.2 order'] 
#x@groupSelectedFrom[['User.group.2']][['grouping']] = x@usedObj$SelectionFiles[['User.group.2']]

dat = x@drc[['DDRtree']][which( x@userGroups[,x@usedObj$lastGroup] == 1 ), ]

t = reduceTo( x, what='col', 'to'= colnames(x@data)[which( x@userGroups[,x@usedObj$lastGroup] == 1 )] )

#t= pseudotimeTest3D( t, dat[,1], dat[,2], dat[,3], x@usedObj$lastGroup )
ofile = file.path( prefix, 'data','output','timeLineTest','2_OneGroupTime_timeSession.html' )
SelectionFile = file.path(x@outpath , paste(gFile, 'time', sep=".") )

if ( file.exists( ofile) ) {
	unlink( ofile )
}
if ( file.exists( SelectionFile)){
	unlink( SelectionFile )
}

t = getDifferentials( t,'User.group.1' ,deg.method= 'wilcox' , Log=FALSE)

expect_true(file.exists( ofile), "new time selection file is missing" )

time= t@usedObj$timelines[['lastEntry']]
o = time$time
d = read.delim(SelectionFile, header=F )


#expect_true( all.equal( as.vector(d[,1]), names(time$c)[o]) == TRUE, "new order was wrong")
expect_true( all.equal( as.vector(d[,1]), names(time$c)[o]) == TRUE, "new order was wrong")
#expect_true( all.equal( as.vector(d[,2]), gplots::bluered( 9 ))==TRUE, "new order")



expect_true( file.exists( ofile), "Rmd (subset) ofile not created" )
expect_true( file.exists( SelectionFile), "Updated selection ofile not created" )

ofile = file.path( prefix, 'data','output','timeLineTest','3_OneGroupTime_timeSession.html' )

if ( file.exists( ofile) ) {
	unlink( ofile )
}
if ( file.exists( SelectionFile)){
	unlink( SelectionFile )
}

a = getDifferentials( x,'User.group.1' ,deg.method= 'wilcox' , Log=FALSE)

expect_true( file.exists( ofile), "Rmd (total) ofile not created" )
expect_true( file.exists( SelectionFile), "Updated selection ofile not created" )
t = table(read.delim( SelectionFile, header=F )[,2])

expect_true(length(t) == 9, paste("too many time colors (", sep="", length(t)," != 9)" ) )
