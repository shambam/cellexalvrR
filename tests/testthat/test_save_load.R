library(cellexalvr)

start_time <- Sys.time()

cellexalObj = loadObject(file.path('data', 'cellexalObj.RData') )

end_time <- Sys.time()

min = end_time - start_time

file.create( file.path('data', 'cellexalObj.RData.lock') )

expect_true(exists('cellexalObj'), 'normal load')

rm(cellexalObj)

start_time <- Sys.time()
tryCatch( {
	cellexalObj =loadObject(file.path('data', 'cellexalObj.RData'), 1 )
},  error = function(err)  {
	#browser()
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

file.remove(file.path('data', 'cellexalObj.RData.lock') )

cellexalObj = loadObject(file.path('data', 'cellexalObj.RData'), 5 )

expect_true(exists('cellexalObj'), 'controlled load no lock file')


