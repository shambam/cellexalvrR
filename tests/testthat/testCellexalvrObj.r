library(cellexalvr)

opath = file.path('data','output')
dir.create(opath,  showWarnings = FALSE)

test_that( "export2cellexalvr function" ,{
load(file.path('data', 'cellexalObj.RData'))
ofiles = c( 'a.meta.cell', 'c.meta.gene', 'database.sql', 'graph1.hull', 'graph1.mds', 'graph2.hull', 'graph2.mds', 'index.facs' )

for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	if(  file.exists(ofile ) ){
		unlink( ofile)
	}
}

export2cellexalvr( cellexalObj , opath )

ofiles = c( 'a.meta.cell', 'c.meta.gene', 'database.sql', 'graph1.hull', 'graph1.mds', 'graph2.hull', 'graph2.mds', 'index.facs' )
#
for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	expect_equal( file.exists( ofile ), TRUE )
}

} ) ## end of test_that


test_that( "heatmap is produced", {
ofile = file.path(opath, 'selection0.png') 
if(  file.exists(ofile ) ){
	unlink( ofile)
}

make.cellexalvr.heatmap ( file.path('data', 'cellexalObj.RData') , file.path('data', 'selection0.txt'), 300, ofile )

expect_equal( file.exists( ofile ), TRUE )


} )


