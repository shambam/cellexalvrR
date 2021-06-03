context('heatmapBug')

prefix= './'
#prefix = 'tests/testthat'

path = file.path( prefix,'data', 'output', 'heatmapBug')

cellexalObj@outpath = path

selection = file.path( prefix,'data', 'selection10.txt')

expect_true( file.exists(selection), label=paste("selection file", selection) )

cellexalObj = getDifferentials( cellexalObj, selection ) ## should be a 3 group selection.

browser()