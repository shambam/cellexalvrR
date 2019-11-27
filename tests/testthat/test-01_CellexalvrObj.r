context('error throwing') 
expect_error( loadObject( 'SomeFileNotExistsing') )

context('export2cellexalvr function')
prefix = '.'
opath = file.path( prefix, 'data','output' )
ipath = file.path( prefix, 'data' )
if ( file.exists(opath)) {
	unlink( opath, recursive=TRUE)
}
dir.create(opath,  showWarnings = FALSE, recursive = TRUE)

context('minimal cellexalvrR object') 
m = matrix( floor(runif(30000, min=-1000, max=101)), ncol=300)
colnames(m) = paste('cell', 1:ncol(m))
rownames(m) = paste('gene', 1:nrow(m))
m[which(m< 1)] = 0
m = Matrix::Matrix(m,sparse=T)
obj = new( 'cellexalvrR', data=m , drc= list('test' = cbind(x=runif(100), y=runif(100), z=runif(100) )) )

export2cellexalvr( obj, opath )


if ( file.exists(file.path(ipath,'cellexalObjOK.RData.lock')) ) {
	unlink(file.path(ipath, 'cellexalObjOK.RData.lock') )
}
if ( ! file.exists (file.path(ipath,'cellexalObjOK.RData') ) ) {
	stop( paste("Libraray error - test file not found ", file.path(ipath,'cellexalObjOK.RData')) )
}
cellexalObj <- loadObject(file.path(ipath,'cellexalObjOK.RData'))

ofiles = c( 'a.meta.cell', 'c.meta.gene', 'database.sqlite', 'DDRtree.mds', 
		 'index.facs',  'diffusion.mds', 'tSNE.mds' )


for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	if(  file.exists(ofile ) ){
		unlink( ofile)
	}
}
if (! file.exists( opath)) {
	dir.create(opath )
	dir.create( file.path(opath, 'default_user') )
	dir.create( file.path(opath, 'default_user','testSession'))
	dir.create( file.path(opath, 'default_user','testSession', 'tables'),  recursive = TRUE)
}

export2cellexalvr(cellexalObj , opath )


for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
}



context( "store user groupings" )

old_length = 0
cellexalObj@userGroups = data.frame()

cellexalObj = userGrouping(cellexalObj, file.path(ipath, 'selection0.txt') )
expect_equal( length(cellexalObj@userGroups) , old_length + 2 )
## but also test whether the file was read correctly!! Epic bug went undetected!!
ids = cellexalObj@userGroups[,1] 
names(ids) = colnames(cellexalObj@data)
orig = read.delim( file.path(ipath, 'selection0.txt'), header=F)
origids = orig[,4] +1
names(origids) = orig[,1]
m = match(names(origids), names(ids) ) ## likely some missing
expect_true( all.equal(origids, ids[m]) == TRUE, 'grouping stored correctly')
## and check the order and the colors, too
expect_true( all.equal(cellexalObj@userGroups[m,2], 1:length(m)) == TRUE, 'order stored correctly' )
expect_true( all.equal(cellexalObj@colors[[1]], as.vector(unique(orig[,2]))) == TRUE, 'color stored correctly' )


cellexalObj = userGrouping(cellexalObj, file.path(opath,'..', 'selection0.txt') )
expect_equal( length(cellexalObj@userGroups) ,old_length +  2 ) # same grouing no adding of the data



context( "heatmap is produced" )
ofile = file.path(opath, 'selection0.png') 
if(  file.exists(ofile ) ){
	unlink( ofile)
}

make.cellexalvr.heatmap.list ( file.path(opath, 'cellexalObj.RData') , file.path(ipath,'selection0.txt'), 300, ofile )

expect_true( file.exists( ofile ),  paste("outfile missing:", ofile) )

