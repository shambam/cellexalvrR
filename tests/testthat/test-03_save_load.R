context('save and load')

opath = file.path('data','output')

test_that( "cellexalvr save and load object" ,{
			datafile = file.path(opath,'..', 'cellexalObj.RData')
			lockfile = file.path(opath,'..', 'cellexalObj.RData.lock')
			start_time <- Sys.time()
			if ( file.exists(lockfile) ) {
				unlink(lockfile)
			}
			cellexalObj2 = loadObject( datafile )
			
			end_time <- Sys.time()
			
			min = end_time - start_time
			
			file.create( lockfile )
			message( "Manually created",lockfile )
			
			expect_true(exists('cellexalObj2'), 'normal load')
			
			rm(cellexalObj2)
			
			# we have one version hardcoded in the source so this will always be true!
			expect_true(! exists('cellexalObj2'), 'just removed the object')
			
			start_time <- Sys.time()
			tryCatch( {
						cellexalObj2 =loadObject( datafile, maxwait= 1 )
					},  error = function(err)  {
						file.remove( lockfile )
						expect_equal( str_extract( as.character(paste(err)), ' Could not obtain access to locked file'),
								" Could not obtain access to locked file",
								'Could not obtain access to locked file with lock')
					} )
			end_time <- Sys.time()
			
			#expect_true( min <  (end_time- start_time ) , 'error with lock took more than 1 sec more')
			expect_true(! exists('cellexalObj2'), 'controlled load - no load due to lock file')
			
			cellexalObj2 = loadObject( datafile , 5 )
			
			expect_true(exists('cellexalObj2'), 'controlled load no lock file')
			
			if ( file.exists(file.path(opath, 'cellexalObj.RData'))){
				file.remove(file.path(opath, 'cellexalObj.RData'))
			}
			
			lockedSave( cellexalObj2, path=opath)
			
			expect_true( file.exists( file.path(opath, 'cellexalObj.RData') ), "lockSave has worked")
			
			expect_true( ! file.exists(file.path(opath, 'cellexalObj.RData.lock')), "lock file removed as expected")
			
			expect_true( ! file.exists( lockfile ), "lock file in the data folder removed as expected")
			
		})