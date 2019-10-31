context('timeline')

prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = read.delim(genes)[,1]

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@outpath = file.path(prefix,'data','output','statTest' )

x = sessionPath( x, 'timelineTest')

grouping <- file.path(prefix, 'data/selection0.txt')

## I need the 3D vectors for the cells in e.g. group 1
x = userGrouping( x, grouping)
#which( x@userGroups[,x@usedObj$lastGroup] == 1 )
x@userGroups[,'User.group.2'] = NA
x@userGroups[,'User.group.2.order'] = x@userGroups[,'User.group.1.order']
x@userGroups[which(x@userGroups[,'User.group.1'] == 2), 'User.group.2'] = 1


x@groupSelectedFrom[['User.group.2']] = x@groupSelectedFrom[['User.group.1']] 


dat = x@drc[['DDRtree']][which( x@userGroups[,x@usedObj$lastGroup] == 2 ), ]

t = reduceTo( x, what='col', 'to'= colnames(x@data)[which( x@userGroups[,x@usedObj$lastGroup] == 2 )] )

t= pseudotimeTest3D( t, dat[,1], dat[,2], dat[,3], x@usedObj$lastGroup )



getDifferentials( t,'User.group.2' ,deg.method= 'wilcox' , Log=FALSE)
