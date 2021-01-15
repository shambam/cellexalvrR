context('timeline - subsets')

#skip('not stable enough')

#prefix = 'tests/testthat'
prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = read.delim(genes)[,1]

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@outpath = file.path(prefix,'data','output','timeLineTest' )

if ( file.exists(x@outpath)){
	dir.create( x@outpath )
}

x = reset(x)
x@outpath = file.path(prefix,'data','output','timeLineTest' )

x = sessionPath( x, 'timeSession_subsets')
expect_true( x@usedObj$sessionName == 'timeSession_subsets',  label='session path not set correctly')
ofile = file.path( prefix, 'data','output','timeLineTest', 'timeSession_subsets', 'AA_Start_paritalLog.Rmd' )
expect_true( file.exists( ofile), label="start session optupt is missing")

ofile = file.path( prefix, 'data','output','timeLineTest', 'timeSession_subsets', 'AB_OneGroupTime_paritalLog.Rmd' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}


gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )


## I need the 3D vectors for the cells in e.g. group 1

x = userGrouping( x, grouping)

x = getDifferentials( x,'User.group.1', deg.method= 'wilcox' , Log=TRUE)

expect_equal( names(x@usedObj$timelines), c("lastEntry", "Time.group.2" ), label="correct time names")

expect_equal(names(x@usedObj$timelines[["Time.group.2"]]@geneClusters[["Time.group.2"]]),
			c("1","2","3","4","5"), label = "geneClusters are part of the timelines" )


subset = rownames(x@usedObj$timelines[["lastEntry"]]@dat)[seq(1,nrow(x@usedObj$timelines[["lastEntry"]]@dat),2)]
timeSubset = subsetTime( x@usedObj$timelines[["lastEntry"]], subset)


x = timeAnalysisSubset( timeSubset, x )

expect_equal( names(x@usedObj$timelines), c("lastEntry", "Time.group.2", "Time.group.3" ), label="correct time names #2")

expect_equal(names(x@usedObj$timelines[["Time.group.3"]]@geneClusters[["Time.group.3"]]),
			c("1","2","3","4","5"), label = "geneClusters are part of the timelines #3" )
browser()
expect_equal(names(x@usedObj$timelines[["Time.group.3"]]@geneClusters[["Time.group.2"]]),
			c("1","2","3","4","5"), label = "geneClusters in #3 also contain #2 genes lists" )

x= renderReport( x )

ofile = file.path( prefix, 'data','output', 'timeLineTest2', 'session-log-for-session-timesession-only-one.html' )
expect_true( file.exists( ofile), label= ofile )

## check the html file for duplicated entries (sections).

collect = list( 
	'TimeLine control from Saved Selection' = 0,
	'as group Time.group.2' = 0,
	'Time.group.2.Linear.csv' = 0,
	'Session Log for Session timeSession_only_one' = 0,
	'2D DRC DDRtree dim 2,3 time line' = 0
)
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
expt = list( 
	'TimeLine control from Saved Selection' = 2, # one in the text and one in the TOC
	'as group Time.group.2' = 1,
	'Time.group.2.Linear.csv' = 1,
	'Session Log for Session timeSession_only_one' = 2, # one in the text and one in the TOC
	'2D DRC DDRtree dim 2,3 time line' = 2 # this figure is shown twice..
)
expect_equal( collect, expt, label="No duplicate entries in the HTMP file")

##################################################
context('timeline check if data is correct')
##################################################

x = check( x ) 
expect_true( x@usedObj$checkPassed, label = x@usedObj$checkError )