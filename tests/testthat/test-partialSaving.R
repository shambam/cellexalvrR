context('partial save process')

prefix = '.'
opath = file.path(prefix,'data','output')

this = cellexalObj ## the inbuil one...
this@outpath = opath
lockedSave( this )

## check the time the load takes:
start_time = Sys.time()
this = loadObject( file.path(opath, 'cellexalObj.RData' ))
end_time = Sys.time()

simpleLoadT = end_time - start_time


## now change something and check if the new loadObject call loads the updated version
skip('unreliable tests')

old_n= ncol(this@userGroups) 
this@userGroups = this@userGroups[,3:4]

savePart( this, 'userGroups' )

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( ncol(this@userGroups) == 2 )

expect_true( length(this@groupSelectedFrom) != 0 )

this@groupSelectedFrom <- list()

savePart( this, 'groupSelectedFrom' )

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( length(this@groupSelectedFrom) == 0 )


this@colors <- list()

savePart( this, 'colors' )

this@colors <- list(a=1, B=2)

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( length(this@colors) == 0 )

this@usedObj$useless_add = NULL

expect_true( is.null(this@usedObj$useless_add)  )

this@usedObj$useless_add = 1

savePart( this, 'usedObj' )

this@usedObj$useless_add = NULL

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true(this@usedObj$useless_add == 1  )

if ( ncol(this@meta.cell) == 21 ){
		this@meta.cell = cbind(this@meta.cell, test= rep('useless1', nrow(this@meta.cell)))
}
this@meta.cell = cbind(this@meta.cell, test= rep('useless', nrow(this@meta.cell)))
savePart( this, 'meta.cell' )
total = ncol(this@meta.cell)

this@meta.cell = this@meta.cell[,1:22]

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( ncol(this@meta.cell) == total )
this@meta.cell = this@meta.cell[,1:22]
savePart( this, 'meta.cell' )

m =  matrix(ncol=0, nrow=0)
storage.mode(m) <- 'double'

expect_equal(this@meta.gene ,  m)

this@meta.gene =as.matrix(data.frame( 'geneID' = rownames(this@data)) )

expect_true( storage.mode(this@meta.gene) == "character" )
expect_true( nrow(this@meta.gene ) == 4709 )

savePart( this, 'meta.gene' )
this@meta.gene = m

start_time = Sys.time()
this = loadObject( file.path(opath, 'cellexalObj.RData' ))
end_time = Sys.time()

expect_true( storage.mode(this@meta.gene) == "character" )
expect_true( nrow(this@meta.gene ) == 4709 )

expect_true ( simpleLoadT > (end_time - start_time) *0.8 )

lockedSave(this)

expect_true( file.exists( file.path(opath, '.userGroups.RData')) == FALSE )

unlink( file.path(opath,'cellexalObj.RData' ))




