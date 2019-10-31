#' This server function is the main speed up for big data files.
#'
#' Instead of loading and saving all files for each function call in the VR process
#' we now can have one R process that sequencially works on all requests.
#'
#' @name server
#' @aliases server,character-method
#' @rdname server-methods
#' @docType methods
#' @description starts a server looking for a sourcable file in paste(file, 'input.R', sep='.')
#' The function sets a paste(file, 'input.lock', sep='.') lock file on processed scripts.
#' To shut down the server process you can either write a q('no') into the script file or remove the pid file.
#' @param file the file core name to create input.R input.log and pid files.
#' @param sleepT sleep time in seconds between checks for the input.R file
#' @keywords server
#' @title start a server function periodicly sourcing in a file.
#' @export server

if ( ! isGeneric('server') ){setGeneric('server', ## Name
			function ( file, sleepT=1 ) { 
				standardGeneric('server') 
			}
	) }

setMethod('server', signature = c ('character'),
		definition =  function(file, sleepT=1){
	lockfile   = paste( file, 'input.lock', sep=".") 
	scriptfile = paste( file, 'input.R', sep="." )
	pidfile    = paste( file, 'pid', sep='.')
	# package version needs to be exported
	pv_file    = paste( file, 'cellexalvrR.version', sep='.')
	file.create(pv_file)
	cat( as.character(packageVersion("cellexalvrR")), file= pv_file, append=F)

	cat( Sys.getpid() , file = pidfile )
	
	## redirect all output to output file
	outFile = file(paste( file,  Sys.getpid(), 'output', sep=".") )

	sink(outFile)

	print ( paste( "server is starting - reading from file:\n", scriptfile))
  	while(TRUE){
		if ( ! file.exists(pidfile ) ) {
			break
		}
        if ( file.exists( scriptfile ) ) {
                while ( file.exists( lockfile ) ) {
                        Sys.sleep( sleepT )
                }
				file.create(lockfile)
                cat ( readLines( scriptfile), file= outFile, sep="\n\r", append=TRUE )
                try ( {source( scriptfile ) } )
                file.remove( scriptfile )
				file.remove(lockfile)
        }
        Sys.sleep( sleepT )   
	} 
	message( "Server pid file lost - closing down" );
	sink()
	close(outFile)

	q('no')
}
)

