write_as_sqlite3 <- function( cellexalObj, ofile ) { 
	
	genes <- rownames(cellexalObj@dat)
	genes <- data.frame( 'id' = 1:length(genes), genes= genes )
	
	cells <- data.frame( 'id'= 1:ncol(cellexalObj@dat), sample= colnames(cellexalObj@dat) )
	
	## melt the sparse matrix using the toColNums Rcpp function
	mdc = FastWilcoxTest::meltSparseMatrix( cellexalObj@dat )
	
	colnames(genes) <- c('id', 'gname')
	colnames(cells) <- c('id','cname')
	
	con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
	
	RSQLite::dbWriteTable(con, "datavalues",mdc)
	
	RSQLite::dbSendStatement(con,"create table genes ('id' integer not null unique,'gname' varchar(20) )")
	
	RSQLite::dbSendStatement(con,"create table cells ('id' integer not null unique,'cname' varchar(20) )")
	
	RSQLite::dbWriteTable(con, "genes", genes, append = TRUE)
	RSQLite::dbWriteTable(con, "cells", cells, append = TRUE)
	
	RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX gnameIDX on genes ( gname )")
	RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX cnameIDX on cells ( cname )")
	
	RSQLite::dbSendStatement(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
	RSQLite::dbSendStatement(con,"create index cell_id_data ON datavalues ( 'cell_id' )")
	
	
	RSQLite::dbDisconnect(con)

}