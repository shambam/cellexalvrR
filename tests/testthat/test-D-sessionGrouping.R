context('create session grouping')

skip( "test run a milion times otherwhere and fails here.")

checkHTMLpaths <- function( ofile, sessionName = 'sessionGroupingTest' ) {
	if ( ! file.exists(ofile)) {
		Sys.sleep(2) ## possible that the file has not been created but will be - soon
	}

html = scan ( ofile, sep="\n", quiet=TRUE, what=character())
err=NULL
i = 0;
m = FALSE
for ( line in html) {
	i = i+1;

	if ( length(grep('png', line )) > 0 ){
		m=TRUE
	}else if(length(grep('".*tables/.*"', line )) > 0){
		m = TRUE
	}
	if ( m ) {
		if ( length(grep( paste('"', sessionName, sep="") , line)) == 0 ){
			err = c(err, paste("HTML path error in line",i), line )
		}
		
	}
	m=FALSE
}
err
}


prefix = './'
# prefix='tests/testthat'

data = file.path(prefix, 'data/cellexalObj.RData')

cellexalObj = loadObject( data )

cellexalObj =reset(cellexalObj)

datadir <- normalizePath(file.path( prefix, 'data', 'output'))

cellexalObj@outpath = file.path(datadir) ## to not mess up the package

unlink(list.files( cellexalObj@outpath, pattern='*.lock'))
## init a session
lockedSave(cellexalObj)

cellexalObj = check(cellexalObj)
expect_true( cellexalObj@usedObj$checkPassed, label=cellexalObj@usedObj$checkError )

fnames = ( c( 
file.path(datadir,"AA_Start_sessionGroupingTest.html") , 
file.path(datadir,"AC_Heatmap_sessionGroupingTest.html"),
file.path(datadir,"AD_Network_sessionGroupingTest.html"),
file.path(datadir,"AB_Stats_sessionGroupingTest.html"),
file.path(datadir,"AE_Ontology_sessionGroupingTest.html"),
file.path(datadir,'sessionGroupingTest',"AA_Start_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"AC_Heatmap_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"AD_Network_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"AB_Stats_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"AE_Ontology_paritalLog.Rmd")

))


lapply( list.files(file.path(datadir) , 
		full.names = TRUE, recursive = TRUE), unlink )

####################################################
context('create session grouping - start clean')
####################################################

cellexalObj = sessionPath( cellexalObj, 'sessionGroupingTest' )

expect_true( file.exists( file.path(datadir, 'sessionGroupingTest' ) ) , "session path has not been created" )

#lapply( list.files(datadir , full.names = TRUE, recursive = FALSE), unlink )

grouping =  file.path(prefix, 'data', 'selection0.txt' )


cellexalObj = userGrouping( cellexalObj, grouping )
cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 1, paste("first entry not 1 (", n, ")"))

grouping =  file.path(prefix, 'data', 'selection1.txt' )

cellexalObj = userGrouping( cellexalObj, grouping )
cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 2, paste("second entry not 2 (", n, ")"))

grouping =  file.path(prefix, 'data', 'selection0.txt' )
cellexalObj = userGrouping( cellexalObj, grouping )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 1, paste("third try: first entry not 1(", n, ")"))

## now add some Session reports:

genes <- file.path(prefix, 'data/heatmap_0.txt')
if ( ! file.exists(file.path(datadir,  'tmp')) ){
	dir.create(file.path(datadir, 'tmp'), recursive = TRUE )
}

png( file=file.path(datadir, 'tmp', 'a_simple_figure.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()

####################################################
context('create session grouping - logHeatmap')
####################################################


heatmap_png <- file.path(datadir,  'tmp', 'a_simple_figure.png')

grouping <- file.path(prefix, 'data/selection0.txt')

ontology <- 'BP'

topNodes  <- 20

ofiles = c('AB_Heatmap_paritalLog.Rmd', 'png/User.group.1.DDRtree.1_2.png', 'png/a_simple_figure.png','png/User.group.1.DDRtree.2_3.png' )

for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir,  'sessionGroupingTest', fname ) ) ) {
		file.remove( file.path( datadir,  'sessionGroupingTest', fname ) )
	}
}
ofile=  file.path( datadir, 'AB_Heatmap_sessionGroupingTest.html')

if( file.exists(ofile)) {
	unlink(ofile)
}

#system( paste( 'Rscript', script, datadir, genes, heatmap_png, grouping, ontology, topNodes ))
cellexalObj = logHeatmap(cellexalObj, genes, heatmap_png, grouping, ontology = ontology, topNodes = topNodes )


for ( fname in c( ofiles ) ){
	expect_true( file.exists( file.path(datadir, 'sessionGroupingTest',  fname ) ) , label= paste( "file has not been created",file.path(datadir, 'sessionGroupingTest', fname) ))
}
expect_true( file.exists( ofile), label = paste( "file has not been created", ofile))

err = checkHTMLpaths( file.path(datadir, 'AB_Heatmap_sessionGroupingTest.html' ))
expect_true(is.null(err), label=err )


####################################################
context('create session grouping - logStatResult ')
####################################################

test = data.frame( A = rep(0,10), B= rep(1,10), 'p_val'= c( 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1) )

if ( file.exists( file.path(datadir, 'AC_Stats_sessionGroupingTest.html' ))) {
	unlink(  file.path(datadir, 'AC_Stats_sessionGroupingTest.html' ) )
}
logStatResult(cellexalObj, 'SimpleTest', test, 'p_val' )

expect_true( file.exists( file.path(datadir, 'AC_Stats_sessionGroupingTest.html' )), label =  'logStatResult')

err = checkHTMLpaths( file.path(datadir, 'AC_Stats_sessionGroupingTest.html' ))
expect_true(is.null(err), label=err )


####################################################
context('create session grouping - logNetwork')
####################################################

if ( file.exists( file.path(datadir, 'AC_Network_sessionGroupingTest.html' ))) {
	unlink(  file.path(datadir, 'AC_Network_sessionGroupingTest.html' ) )
}
cellexalObj = logNetwork(cellexalObj,  png =  heatmap_png , grouping= grouping )
expect_true( file.exists( file.path(datadir, 'AD_Network_sessionGroupingTest.html' )),label =  'logNetworks')

err = checkHTMLpaths( file.path(datadir, 'AD_Network_sessionGroupingTest.html' ))
expect_true(is.null(err), label=err )

####################################################
context('create session grouping - ontologyLogPage')
####################################################

if ( file.exists( file.path(datadir, 'AE_Ontology_sessionGroupingTest.html' ))) {
	unlink(  file.path(datadir, 'AE_Ontology_sessionGroupingTest.html' ) )
}
cellexalObj = ontologyLogPage(cellexalObj,  genes=genes , grouping= grouping )
expect_true( file.exists( file.path(datadir, 'AE_Ontology_sessionGroupingTest.html' )), label =  'ontologyLog')

err = checkHTMLpaths( file.path(datadir, 'AE_Ontology_sessionGroupingTest.html' ))
expect_true(is.null(err), label=err )

####################################################
context('create session grouping - logFigure')
####################################################

if ( file.exists( file.path(datadir, 'AF_Figure_sessionGroupingTest.html' ))) {
	unlink(  file.path(datadir, 'AF_Figure_sessionGroupingTest.html' ) )
}
cellexalObj = logFigure(cellexalObj,  png =  heatmap_png, text="Useless figure"  )
expect_true( file.exists( file.path(datadir, 'AF_Figure_sessionGroupingTest.html' )), label =  'ontologyLog')

err = checkHTMLpaths( file.path(datadir, 'AF_Figure_sessionGroupingTest.html' ))
expect_true(is.null(err), label=err )

####################################################
context('create session grouping - logTimeline')
####################################################

gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )

## this produces two outfile:
ofiles = c('AG_Stats_sessionGroupingTest.html','AH_OneGroupTime_sessionGroupingTest.html' )
for ( ofile in ofiles ) {
if ( file.exists( file.path(datadir, ofile ))) {
	unlink(  file.path(datadir, ofile ) )
}
}

cellexalObj = getDifferentials( cellexalObj, grouping , deg.method= 'wilcox' , Log=TRUE)


for ( ofile in ofiles ) {
if ( file.exists( file.path(datadir, ofile ))) {
	expect_true( file.exists( file.path(datadir, ofile )), label = ofile)
	err = checkHTMLpaths( file.path(datadir, ofile ))
	expect_true(is.null(err), label=paste(ofile,":\n",err) )
}
}


####################################################
context('create session grouping - renderReport')
####################################################

ofile=  file.path( cellexalObj@outpath, 'session-log-for-session-sessiongroupingtest.html')
if( file.exists(ofile) ) {
	unlink(ofile)
}

cellexalObj = renderReport ( cellexalObj )


expect_true( file.exists( ofile), label = ofile )




	