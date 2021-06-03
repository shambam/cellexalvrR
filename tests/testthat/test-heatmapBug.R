context('heatmapBug')

prefix= './'
#prefix = 'tests/testthat'

path = file.path( prefix,'data', 'output', 'heatmapBug')
if ( file.exists( path)) {
	unlink(path, recursive=TRUE)
}
dir.create(path)

cellexalObj@outpath = path

cellexalObj= sessionPath( cellexalObj, 'test')
selection = file.path( prefix,'data', 'selection11.txt')

expect_true( file.exists(selection), label=paste("selection file", selection) )

#cellexalObj = getDifferentials( cellexalObj, selection ) ## should be a 3 group selection.
dir.create( file.path( path, 'Heatmaps'))
make.cellexalvr.heatmap.list(cellexalObj, cellidfile = selection, num.sig=250, outfile= file.path(path, 'Heatmaps' , 'TestHeatmap') )

files = c(  "AA_Start_test.html","AB_Stats_test.html", "AC_OneGroupTime_test.html",
  "cellexalObj.RData", "Heatmaps", "mainServer.sessionName",
  "selection11.txt.time", "selection11.txt.time.points", "test" )


expect_equal(list.files(path), files, label="All expected outfiles #1" )

files = c("TestHeatmap", "TestHeatmap.sqlite3" )
expect_equal( list.files(file.path(path, 'Heatmaps')), files , label="Heatmap VR files" )

cellexalObj = renderReport( cellexalObj )

browser()

files = c( "cellexalObj.RData", "libs", "PortableLog_test.zip", 
	"reference-keys.txt", "search_index.json", 
	"session-log-for-session-test.html", "test")

expect_equal(list.files(path), files, label="All expected outfiles #2" )
