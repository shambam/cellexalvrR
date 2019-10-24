Rscript.exe <- function( ) {
	rscript = file.path( R.home(), 'bin', 'Rscript')
	if (! file.exists(rscript ) ) {
		rscript = paste( rscript, sep=".", 'exe')
	}
	return( paste( '"',rscript,'"', sep="" ))
}

R.exe <- function( ) {
	rscript = file.path( R.home(), 'bin', 'R')
	if (! file.exists(rscript ) ) {
		rscript = paste( rscript, sep=".", 'exe')
	}
	return( paste( '"',rscript,'"', sep="" ) )
}

file2Script <- function( path, mustWork = FALSE) {
    output <- c(strsplit(dirname(normalizePath(path,mustWork = mustWork)),
                       .Platform$file.sep )[[1]], basename(path))
    paste("file.path('",paste( output, collapse="','"),"')", sep="")
}