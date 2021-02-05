#' checkFile checks a test file and fetches the amount of matches per string.

checkFile = function ( collect, ofile ) {

  if ( ! file.exists( ofile) ) {
    stop(paste( "the outfile", ofile,"does not exist and can therefore not be examined."))
  }
	con = file(ofile, "r")
	while ( TRUE ) {
  	line = readLines(con, n = 1)
 	if ( length(line) == 0 ) {
   		break
  	}
  	for ( na in names(collect) ){
  		if ( length(grep( na, line))> 0){
  			collect[[na]] = collect[[na]] +1
  		}
  	}
	}
	close(con)

	collect
}
