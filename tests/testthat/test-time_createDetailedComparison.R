context('timeline - createDetailedComparison')

#skip('not stable enough')

#prefix = 'tests/testthat'
prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = read.delim(genes)[,1]

checkFile = function ( collect, ofile ) {

	con = file(ofile, "r")
	while ( TRUE ) {
  	line = readLines(con, n = 1)
 	if ( length(line) == 0 ) {
   		break
  	}
  	for ( na in names(collect) ){
  		if ( length(grep( na, line))> 0){
  			collect[[na]] = collect[[na]] +1
  		}
  	}
	}
	close(con)

	collect
}


cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@outpath = file.path(prefix,'data','output','timeLineTest' )

if ( ! file.exists(x@outpath)){
	dir.create( x@outpath )
}

x = reset(x)
x@outpath = file.path(prefix,'data','output','timeLineTest' )
sessionPath = file.path(prefix,'data','output','timeLineTest', 'timeSession_detailed' )
if ( file.exists( sessionPath )){
	unlink( sessionPath, recursive=T )
}

x = sessionPath( x, 'timeSession_detailed')
expect_true( x@usedObj$sessionName == 'timeSession_detailed', 
 label='session path not set correctly')

ofile = file.path( prefix, 'data','output','timeLineTest', 
	'timeSession_detailed', 'AA_Start_paritalLog.Rmd' )
expect_true( file.exists( ofile), label="start session optupt is missing")

ofile = file.path( prefix, 'data','output','timeLineTest', 
	'timeSession_detailed', 'AB_OneGroupTime_paritalLog.Rmd' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}

gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )


x= userGrouping( x, grouping )
x= pseudotimeTest3D( x, grouping = NULL )

expect_equal( names(x@usedObj$timelines), c('lastEntry', 'Time.group.2') ,
 label="right timeline names" )

time = x@usedObj$timelines[[ 'Time.group.2' ]]
x = createStats( time, x )

ofile=file.path(x@usedObj$sessionPath, '..', 'AB_Stats_timeSession_detailed.html')

collect = list( 
	'<td>16</td>' = 0,
	'2D DRC DDRtree dim 1,2' = 0,
	'2D DRC DDRtree dim 2,3' = 0
	)
expt = list(
	'<td>16</td>' = 9,
	'2D DRC DDRtree dim 1,2' = 1,
	'2D DRC DDRtree dim 2,3' = 1
	)
expt[[x@outpath]] = 0
collect[[x@outpath]] = 0

collect = checkFile ( collect, ofile )

expect_equal( collect, expt, label="stats report as expected")


deg.genes = x@usedObj$deg.genes

subsets = list( 
  'A' = rownames(time@dat)[seq(1,nrow(time@dat),2)],
  'B' = rownames(time@dat)[seq(2,nrow(time@dat),2)]
)



x = createDetailedComparison(time,  x, deg.genes = deg.genes, subsets=subsets, name="FirstTest" )

ofile = file.path(x@outpath, 'session-log-for-session-timesession-detailed.html')
if ( file.exists( ofile)) {
	unlink ( ofile)
}

x = renderReport(x)

expect_true( file.exists(ofile), label="the detailed comparison html report file exists" )


collect= list( 
	'2D DRC DDRtree dim 1,2' = 0, # one in the text and one in the TOC
	'2D DRC DDRtree dim 2,3' = 0,
#	'Time.group.2.Linear.csv' = 1,
#	'Session Log for Session timeSession_subsets' = 2, # one in the text and one in the TOC
#	'2D DRC DDRtree dim 2,3 time line' = 4, # this figure is shown twice..
	'href="https://www.genecards.org/cgi-bin/' = 0 # two 250 gene lists.
)
expt = list( 
	'2D DRC DDRtree dim 1,2' = 4, # one in the text and one in the TOC
	'2D DRC DDRtree dim 2,3' = 4,
#	'Time.group.2.Linear.csv' = 1,
#	'Session Log for Session timeSession_subsets' = 2, # one in the text and one in the TOC
#	'2D DRC DDRtree dim 2,3 time line' = 4, # this figure is shown twice..
	'href="https://www.genecards.org/cgi-bin/' = 250 # two 250 gene lists.
)
expt[[x@outpath]] = 0
collect[[x@outpath]] = 0

collect = checkFile ( collect, ofile )

expect_equal( collect, expt, label="No duplicate entries in the HTMP file")







