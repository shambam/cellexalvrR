
opath = file.path('data','output')
dir.create(opath,  showWarnings = FALSE)
cellexalObj <- loadObject(file.path(opath,'..', 'cellexalObj.RData'))

test_that( "export2cellexalvr function" ,{

			ofiles = c( 'a.meta.cell', 'c.meta.gene', 'database.sqlite', 'graph1.hull', 'graph1.mds', 'graph2.hull', 'graph2.mds', 'index.facs' )
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				if(  file.exists(ofile ) ){
					unlink( ofile)
				}
			}
			if (! file.exists( opath)) {
				dir.create(opath )
			}
			export2cellexalvr( cellexalObj , opath )
			

			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
			
		} ) ## end of test_that


test_that( "store user groupings" ,{
			old_length = 0
			if ( length(cellexalObj@userGroups) > 0 ){
				old_length = length(cellexalObj@userGroups) -2 ## and therefore a pointless test...
			}
			cellexalObj = userGrouping(cellexalObj, file.path(opath,'..', 'selection0.txt') )
			expect_equal( length(cellexalObj@userGroups) , old_length + 2 )
			
			cellexalObj = userGrouping(cellexalObj, file.path(opath,'..', 'selection0.txt') )
			expect_equal( length(cellexalObj@userGroups) ,old_length +  2 ) # same grouing no adding of the data
			
		} ) ## end store user groupings


test_that( "heatmap is produced", {
			ofile = file.path(opath, 'selection0.png') 
			if(  file.exists(ofile ) ){
				unlink( ofile)
			}
			
			make.cellexalvr.heatmap ( file.path('data', 'cellexalObj.RData') , file.path(opath,'..','selection0.txt'), 300, ofile )
			
			expect_true( file.exists( ofile ),  paste("outfile exists", ofile) )
			
			
		} )


