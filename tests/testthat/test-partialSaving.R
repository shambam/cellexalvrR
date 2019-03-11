context('partial save process')

prefix = './'
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

expect_true( ncol(this@userGroups) == 10 )
this@userGroups = this@userGroups[,1:4]

savePart( this, 'userGroups' )

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( ncol(this@userGroups) == 4 )

expect_true( length(this@groupSelectedFrom) == 2 )

this@groupSelectedFrom <- list()

savePart( this, 'groupSelectedFrom' )

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( length(this@groupSelectedFrom) == 0 )

expect_true( length(this@colors) == 5 )

this@colors <- list()

savePart( this, 'colors' )

this@colors <- list(a=1, B=2)

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( length(this@colors) == 0 )

expect_true( is.null(this@usedObj$useless_add)  )
this@usedObj$useless_add = 1

savePart( this, 'usedObj' )

this@usedObj$useless_add = NULL

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true(this@usedObj$useless_add == 1  )

expect_true( ncol(this@meta.cell) == 23 )
this@meta.cell = cbind(this@meta.cell, test= rep('useless', nrow(this@meta.cell)))
savePart( this, 'meta.cell' )

this@meta.cell = this@meta.cell[,1:23]

this = loadObject( file.path(opath, 'cellexalObj.RData' ))

expect_true( ncol(this@meta.cell) == 24 )

m =  matrix(ncol=0, nrow=0)
storage.mode(m) <- 'double'

expect_equal(this@meta.gene ,  m)

this@meta.gene =as.matrix(data.frame( 'geneID' = rownames(this@dat)) )

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



