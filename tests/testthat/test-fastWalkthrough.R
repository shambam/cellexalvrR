context('fast walk through')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

## structure - get the object and start a session
## use the inbuilt object
prefix = './'
opath = file.path(prefix,'data','output')

load( file.path(opath,'..', 'cellexalObj.RData') )
cellexalObj@userGroups=data.frame()

geneT = read.delim( file.path(opath,'..', 'heatmap_0.txt') )

cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs' )

genes = unique( c( as.vector(geneT[,1]), rownames(cellexalObj@data)[which( is.na(cellexalObj@meta.gene[,'TFs']) == F)] ))

cellexalObj = reduceTo( cellexalObj, what='row', to=genes )

cells = scan( file.path(prefix, 'data', 'cells.txt.sorted'), what=character() ) ## all cells tiouched by the selection0 and selection1

cellexalObj = reduceTo( cellexalObj, what='col', to=cells )

cellexalObj@outpath = opath
lockedSave( cellexalObj ) ## the data folder representation

if ( file.exists(file.path(opath, 'smallObj'))){
	unlink(file.path(opath, 'smallObj'), recursive=TRUE )
}
dir.create( file.path(opath, 'smallObj') )

cellexalObj@outpath = file.path(opath, 'smallObj')
lockedSave( cellexalObj ) ## the user folder representation

expect_true( file.exists( file.path( cellexalObj@outpath, 'cellexalObj.RData')), paste("file not saved", file.path( cellexalObj@outpath, 'cellexalObj.RData')))

## now we have a small object with all gene samples but way less genes.
## this translates to - we can use all input files anyhow!

context('fast walk through - start session')

## start the session:
prefix = './'


script = file.path(prefix, 'data/vrscripts/logStart.R')

datadir <- cellexalObj@outpath ## please give me the user spcific analysis path here!!!!

sessionString <- 'testSession'

if ( file.exists( file.path(datadir,'testSession' ) )){
	unlink ( file.path(datadir,'testSession' ), recursive = TRUE )
}

system( paste( 'Rscript', script, datadir, sessionString  ) )

opaths <- c( 'testSession', 'testSession/png','testSession/tables',  'testSession/00.SessionStart.Rmd')
for ( fname in opaths){
	expect_true( file.exists( file.path(datadir,  fname ) ) , paste( "file has not been created", fname) )
}

cellexalObj <- loadObject( file.path( cellexalObj@outpath, 'cellexalObj.RData') )## should have been updated

expect_true( cellexalObj@usedObj$sessionPath ==  normalizePath(file.path(datadir,'testSession')), paste( cellexalObj@usedObj$sessionPath ,"!=",  file.path(datadir,'testSession')) )

## OK this has hopefully worked

context('fast walk through - register grouping 1')

script = file.path(prefix, 'data/vrscripts/update_grouping.R')

#selectionfile <- args[1]
#userfolder <- args[2]
#datafolder <- args[3]

selectionfile = file.path(prefix, 'data/selection0.txt')
userfolder = file.path(opath, 'smallObj')
datafolder = opath

system( paste( 'Rscript', script, selectionfile, userfolder,  datafolder ) )

## this should have not touched the session information

cellexalObj <- loadObject( file.path( cellexalObj@outpath, 'cellexalObj.RData') )## should have been updated

expect_true( cellexalObj@usedObj$lastGroup == 'User.group.1' , paste( "user group is not 'User.Group.1'", cellexalObj@usedObj$lastGroup) )

expect_true( cellexalObj@usedObj$sessionPath == normalizePath( file.path(datadir,'testSession')), paste( "session path changed:", cellexalObj@usedObj$sessionPath ,"!=",  file.path(datadir,'testSession')) )

context('fast walk through - createNetwork' )

script = file.path(prefix, 'data/vrscripts/make_networks.R')
input_file <- file.path(prefix, 'data/selection0.txt') # grouping file path
datadir <- file.path(prefix, 'data/output/smallObj' ) # the user specific folder
output_file  <-  file.path(prefix, 'data/output/smallObj' ) # the output path

system( paste( 'Rscript', script, input_file,  datadir, output_file ) )


cellexalObj <- loadObject( file.path( cellexalObj@outpath, 'cellexalObj.RData') )## should have been updated

expect_true( cellexalObj@usedObj$lastGroup == 'User.group.1' , paste( "user group is not 'User.Group.1'", cellexalObj@usedObj$lastGroup) )

expect_true( cellexalObj@usedObj$sessionPath == normalizePath( file.path(datadir,'testSession')), paste( "session path changed:", cellexalObj@usedObj$sessionPath ,"!=",  file.path(datadir,'testSession')) )



context('fast walk through - logNetwork' )

script = file.path(prefix, 'data/vrscripts/logNetwork.R')
datadir <- file.path(prefix, 'data/output/smallObj' ) ## please give me the user spcific analysis path here!!!!

dir.create( file.path(datadir, 'tmp') )
png( file=file.path(datadir, 'tmp', 'a_simple_figure2.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()
heatmap_png <- file.path(datadir, 'tmp', 'a_simple_figure2.png')
grouping <- file.path(prefix, 'data/selection0.txt')

ofiles = c( file.path('png', 'a_simple_figure2.png'), 'png/User.group.1.graph1.2_3.png', 'png/User.group.1.graph1.1_2.png'  )
for ( fname in ofiles ){
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}
system( paste( 'Rscript', script, datadir, heatmap_png, grouping  ) )

for ( fname in ofiles){
	expect_true( file.exists( file.path(datadir, 'testSession',  fname ) ) , paste( "file has not been created", fname) )
}


context('fast walk through - register grouping 2')

script = file.path(prefix, 'data/vrscripts/update_grouping.R')

#selectionfile <- args[1]
#userfolder <- args[2]
#datafolder <- args[3]

selectionfile = file.path(prefix, 'data/selection1.txt')
userfolder = file.path(opath, 'smallObj')
datafolder = opath

system( paste( 'Rscript', script, selectionfile, userfolder,  datafolder ) )

## this should have not touched the session information

cellexalObj <- loadObject( file.path( cellexalObj@outpath, 'cellexalObj.RData') )## should have been updated

expect_true( cellexalObj@usedObj$lastGroup == 'User.group.2' , paste( "user group is not 'User.group.2'", cellexalObj@usedObj$lastGroup) )

expect_true( cellexalObj@usedObj$sessionPath == normalizePath( file.path(datadir,'testSession')), paste( "session path changed:", cellexalObj@usedObj$sessionPath ,"!=",  file.path(datadir,'testSession')) )

context('fast walk through - createNetwork 2' )

script = file.path(prefix, 'data/vrscripts/make_networks.R')
input_file <- file.path(prefix, 'data/selection1.txt') # grouping file path
datadir <- file.path(prefix, 'data/output/smallObj' ) # the user specific folder
output_file  <-  file.path(prefix, 'data/output/smallObj' ) # the output path

system( paste( 'Rscript', script, input_file,  datadir, output_file ) )


cellexalObj <- loadObject( file.path( cellexalObj@outpath, 'cellexalObj.RData') )## should have been updated
expect_true( cellexalObj@usedObj$lastGroup == 'User.group.2' , paste( "user group is not 'User.group.2'", cellexalObj@usedObj$lastGroup) )
expect_true( cellexalObj@usedObj$sessionPath == normalizePath( file.path(datadir,'testSession')), paste( "session path changed:", cellexalObj@usedObj$sessionPath ,"!=",  file.path(datadir,'testSession')) )



context('fast walk through - logNetwork 2' )

script = file.path(prefix, 'data/vrscripts/logNetwork.R')
datadir <- file.path(prefix, 'data/output/smallObj' ) ## please give me the user spcific analysis path here!!!!

png( file=file.path(datadir, 'tmp', 'a_simple_figure2.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()
heatmap_png <- file.path(datadir, 'tmp', 'a_simple_figure2.png')
grouping <- file.path(prefix, 'data/selection1.txt')

ofiles = c( file.path('png', 'a_simple_figure2.png'), 'png/User.group.2.graph1.2_3.png', 'png/User.group.2.graph1.1_2.png'  )
for ( fname in ofiles ){
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}
system( paste( 'Rscript', script, datadir, heatmap_png, grouping  ) )

for ( fname in ofiles){
	expect_true( file.exists( file.path(datadir, 'testSession',  fname ) ) , paste( "file has not been created", fname) )
}


context('fast walk through - create heatmap (1)' )


script = file.path(prefix, 'data/vrscripts/make_heatmap.R')

homedir <- file.path(prefix, 'data/output/smallObj','output' ) # <user specific folder>/output
datadir <- file.path(prefix, 'data/output/smallObj' )# <user specific folder>
latest_version <- file.path(prefix, 'data/selection0.txt') # filepath to the grouping file
output_filepath <- file.path(prefix, 'data/output/smallObj' ,'heatmap_genes1.txt') # <homedir>/<heatmapName>.txt
top_genes_number <- 250 # integer norm 250

ofiles = c( 'heatmap_genes1.txt'  )
for ( fname in ofiles ){
	if( file.exists( file.path( datadir, fname ) ) ) {
		file.remove( file.path( datadir,  fname ) )
	}
}

system( paste( 'Rscript', script, homedir, datadir, latest_version, output_filepath, top_genes_number ) )
for ( fname in ofiles){
	expect_true( file.exists( file.path(datadir,  fname ) ) , paste( "file has not been created", fname) )
}

cellexalObj <- loadObject( file.path( cellexalObj@outpath, 'cellexalObj.RData') )## should have been updated
expect_true( cellexalObj@usedObj$lastGroup == 'User.group.2' , paste( "user group is not 'User.group.2'", cellexalObj@usedObj$lastGroup) )
expect_true( cellexalObj@usedObj$sessionPath == normalizePath( file.path(datadir,'testSession')), paste( "session path changed:", cellexalObj@usedObj$sessionPath ,"!=",  file.path(datadir,'testSession')) )

context('fast walk through - log heatmap (1)' )


if ( ! file.exists(file.path(datadir, 'tmp')) ){
	dir.create(file.path(datadir, 'tmp'), recursive = TRUE )
}

script = file.path(prefix, 'data/vrscripts/logHeatmap.R')


datadir <- file.path(prefix, 'data/output/smallObj' ) ## please give me the user spcific analysis path here!!!!
genes <-  file.path(prefix, 'data/heatmap_0.txt')  ## the heatmap_<x>.txt file
heatmap_png <- file.path(datadir, 'tmp', 'a_simple_figure.png') ## the heatmap figure file
grouping <- file.path(prefix, 'data/selection0.txt') ## the grouping info selection0.txt or so
topNodes = 250


png( file=heatmap_png, width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()

ofiles = c( 'png/User.group.1.graph1.1_2.png', 'png/a_simple_figure.png','png/User.group.1.graph1.2_3.png' )

for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir, 'testSession', fname ) ) ) {
		file.remove( file.path( datadir, 'testSession', fname ) )
	}
}
ontology = 'BP'
system( paste( 'Rscript', script, datadir, genes, heatmap_png, grouping, ontology, topNodes ))

for ( fname in c( ofiles, '00.SessionStart.Rmd') ){
	expect_true( file.exists( file.path(datadir, 'testSession',  fname ) ) , paste( "file has not been created",file.path(datadir, 'testSession', fname) ))
}




context('fast walk through - finalze log')

prefix = './'

script = file.path(prefix, 'data/vrscripts/logStop.R')

ofiles <- c( 'testSession/_bookdown.yml', 'session-log-for-session-testsession.html', 'search_index.json' )

for ( fname in ofiles ) {
	if (  file.exists( file.path(datadir,  fname ) ) ){
		file.remove( file.path(datadir,  fname ) )
	}	
}

system( paste( 'Rscript', script, datadir  ) )

test <- loadObject(file.path(datadir, "cellexalObj.RData"))

expect_true( is.null(test@usedObj$sessionName) , "session has ended" )


for ( fname in ofiles ){
	expect_true( file.exists( file.path(datadir, fname ) ) , paste( "file has not been created",  file.path(datadir,fname) ))
}



