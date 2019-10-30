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

dat = x@drc[['DDRtree']][which( x@userGroups[,x@usedObj$lastGroup] == 2 ), ]

t = reduceTo( x, what='col', 'to'= colnames(x@data)[which( x@userGroups[,x@usedObj$lastGroup] == 2 )] )

t= pseudotimeTest3D( t, dat[,1], dat[,2], dat[,3], x@usedObj$lastGroup )

