
opath = file.path('data','output')
#opath = 'tests/testthat/data/output/' ## for manual debugging

if ( ! file.exists(opath) )
	dir.create( opath)

cellexalObj = loadObject(file.path(opath, '..', 'cellexalObj.RData') )
cellexalObj@usedObj$sessionName = NULL

cellexalObj@outpath = opath

cellexalObj = sessionPath( cellexalObj, 'testSession' )

expect_true( file.path( opath, 'testSession' ) == cellexalObj@usedObj$sessionPath ) 

test_that( "cellexalvrR Heatmap report" ,{
			
			ofiles = c('a_simple_figure.png',
					unlist(lapply(c('0.GOanalysis.Rmd', '0.Heatmap.Rmd'), function(x)  file.path( 'testSession', x) ) ),
					unlist(lapply(c( 'a_simple_figure.png', 'User.group.1.graph1.1_2.png', 'User.group.1.graph1.2_3.png' ), 
									function(x)  file.path( 'testSession', 'png',  x) ) ),
					unlist(lapply(c( '0.GOanalysis.csv', '0.GOgenes.csv'), function(x)  file.path( 'testSession', 'tables',  x) ) )
			)
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				if(  file.exists(ofile ) ){
					unlink( ofile)
				}
			}
			
			png( file=file.path(opath, 'a_simple_figure.png'), width=800, height=800 )
			plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
			dev.off()
			
			cellexalObj = logHeatmap ( cellexalObj, png=file.path(opath, 'a_simple_figure.png'), 
					genes=c( file.path(opath, '..','heatmap_0.txt') ) , 
					grouping = file.path(opath, '..', 'selection0.txt')  
			)
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
			
		}
)
#cellexalObj = loadObject(file.path(opath, '..', 'cellexalObj.RData') )


test_that( "cellexalvrR Network report" ,{
			
			ofiles = c('a_simple_figure2.png',
					unlist(lapply(c( '0.Network.Rmd'), function(x)  file.path( 'testSession', x) ) ),
					unlist(lapply(c( 'a_simple_figure2.png', 'User.group.2.graph1.1_2.png', 'User.group.2.graph1.2_3.png' ), 
									function(x)  file.path( 'testSession', 'png',  x) ) )
			)
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				if(  file.exists(ofile ) ){
					unlink( ofile)
				}
			}
			
			#genes = file.path('data','heatmap_0.txt')
			png( file=file.path(opath, 'a_simple_figure2.png'), width=800, height=800 )
			plot(1:100, sample(100:1, 100), main="Just for the test 2!" )
			dev.off()
			
			cellexalObj = logNetwork ( cellexalObj, png=file.path(opath, 'a_simple_figure2.png'), grouping = file.path(opath, '..', 'selection1.txt')  )
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
			
		}
)

#cellexalObj = loadObject(file.path(opath, '..', 'cellexalObj.RData') )


test_that( "cellexalvrR report generation" ,{
			
			ofiles = c('testSession.html')
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				if(  file.exists(ofile ) ){
					unlink( ofile)
				}
			}
			
			#genes = file.path('data','heatmap_0.txt')
			cellexalObj.f = renderReport ( cellexalObj )
			
			for ( f in ofiles ) {
				ofile = file.path(opath, f ) 
				expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
			}
		}
)

