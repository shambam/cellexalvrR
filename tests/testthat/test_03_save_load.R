library(cellexalvr)

opath = file.path('data','output')

test_that( "cellexalvr save and load object" ,{
			
			start_time <- Sys.time()
			
			cellexalObj = loadObject(file.path('data', 'cellexalObj.RData') )
			
			end_time <- Sys.time()
			
			min = end_time - start_time
			
			file.create( file.path('data', 'cellexalObj.RData.lock') )
			write( "Manually created",file.path('data', 'cellexalObj.RData.lock') )
			
			expect_true(exists('cellexalObj'), 'normal load')
			
			rm(cellexalObj)
			
			expect_true(! exists('cellexalObj'), 'just removed the object')
			
			start_time <- Sys.time()
			tryCatch( {
						cellexalObj =loadObject(file.path('data', 'cellexalObj.RData'), 1 )
					},  error = function(err)  {
						#browser()
						file.remove(file.path('data', 'cellexalObj.RData.lock') )
						expect_equal( as.character(paste(err)),
								paste( 
										"Error in loadObject(file.path(\"data\", \"cellexalObj.RData\"), 1):",
										"Could not obtain access to locked file",
										file.path('data', 'cellexalObj.RData\n') 
								), 
								'Could not obtain access to locked file with lock')
					} )
			end_time <- Sys.time()
			
			expect_true( min <  (end_time- start_time ) , 'error with lock took mote than 1 sec more')
			expect_true(! exists('cellexalObj'), 'controlled load - no load due to lock file')
			
			cellexalObj = loadObject(file.path('data', 'cellexalObj.RData'), 5 )
			
			expect_true(exists('cellexalObj'), 'controlled load no lock file')
			
			if ( file.exists(file.path(opath, 'cellexalObj.RData'))){
				file.remove(file.path(opath, 'cellexalObj.RData'))
			}
			
			lockedSave( cellexalObj, path=opath)
			
			expect_true( file.exists(file.path(opath, 'cellexalObj.RData')), "lockSave has worked")
			
			expect_true( ! file.exists(file.path(opath, 'cellexalObj.RData.lock')), "lock file removed as expected")
			
			expect_true( ! file.exists(file.path('data', 'cellexalObj.RData.lock')), "lock file in the data folder removed as expected")
			
		})