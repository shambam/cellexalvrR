
opath = file.path('data','output')
#opath = 'tests/testthat/data/output/' ## for manual debugging

if ( ! file.exists(opath) )
	dir.create( opath)

cellexalObj = loadObject(file.path(opath, '..', 'cellexalObj.RData') )
cellexalObj@usedObj$sessionName = NULL

cellexalObj@outpath = opath

(cellexalObj = sessionPath( cellexalObj, 'testSession' ))

test_that( "cellexalvrR Heatmap report" ,{
			
			ofiles = c('a_simple_figure.png')
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				if(  file.exists(ofile ) ){
					unlink( ofile)
				}
			}
			
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
			
		}
)
cellexalObj = loadObject(file.path(opath, '..', 'cellexalObj.RData') )


test_that( "cellexalvrR Network report" ,{
			
			ofiles = c()
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				if(  file.exists(ofile ) ){
					unlink( ofile)
				}
			}
			
			#genes = file.path('data','heatmap_0.txt')
			png( file=file.path(opath, 'a_simple_figure2.png'), width=800, height=800 )
			plot(1:100, sample(100:1, 100), main="Just for the test 2!" )
			logNetwork ( cellexalObj, png=file.path(opath, 'a_simple_figure2.png'), grouping = file.path(opath, '..', 'selection0.txt')  )
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
			
		}
)

cellexalObj = loadObject(file.path(opath, '..', 'cellexalObj.RData') )


test_that( "cellexalvrR report generation" ,{
			
			ofiles = c()
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				if(  file.exists(ofile ) ){
					unlink( ofile)
				}
			}
			
			#genes = file.path('data','heatmap_0.txt')
			renderReport ( cellexalObj )
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
		}
)

