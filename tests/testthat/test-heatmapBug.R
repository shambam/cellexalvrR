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
selection = file.path( prefix,'data', 'selection10.txt')

expect_true( file.exists(selection), label=paste("selection file", selection) )

cellexalObj = getDifferentials( cellexalObj, selection ) ## should be a 3 group selection.

files = c( "AA_Start_test.html", "AB_Stats_test.html", "mainServer.sessionName", "test")

expect_equal(list.files(path), files, label="All expected outfiles #1" )

cellexalObj = renderReport( cellexalObj )


files = c( "cellexalObj.RData", "libs", "PortableLog_test.zip", 
	"reference-keys.txt", "search_index.json", 
	"session-log-for-session-test.html", "test")

expect_equal(list.files(path), files, label="All expected outfiles #2" )
