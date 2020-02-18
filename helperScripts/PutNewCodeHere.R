
export_GOIs <- function(x, GOIs, grouping, path ) {
	if ( is.null( x@samples[,grouping] )){
		stop(paste("Grouping", grouping,"not in the object!") )
	}
	m = match(GOIs, rownames(x))
	if ( length(which(is.na(m))) > 0 ){
		stop(paste("These genes are not in the object:", paste(collapse=", ", GOIs[which(is.na(m))])))
	}
	if ( ! file.exists(path) ){
		stop(paste("Path",path, "does not exist!" ))
	}
	gene_cuts <- split(GOIs, ceiling(seq_along(GOIs)/250))
	## the drc the groups have been selected from is unknown - take the first one...
	cellidfile = file.path( path, "GOIs_selection.txt")
	# name color drcName id - no colnames!
	cols = rainbow(length(table(x@samples[,grouping])))[x@samples[,grouping]]
	write.table( data.frame( colnames(x@data), cols, rep(names(x$drc)[1], nrow(x@samples)), x@samples[,grouping] ), 
		file= cellidfile, quote=FALSE, row.names=FALSE, sep="\t")

	for ( i in 1:length(gene_cuts)) {
			outfile=file.path( path,paste(sep="", "GOIS_slice_",i,".txt") )
			x@groupSelectedFrom[[x@usedObj$lastGroup]][["heatmapBasename"]] = basename( cellidfile )
			gene.cluster.order = gene_cuts[[i]]

			message (paste( "trying to write file", outfile, "containing", length(gene.cluster.order), "genes") )
			write(c(length(gene.cluster.order),gene.cluster.order),file=outfile,ncolumns=1)
			## probably a good way to export the information as database, too.
			## we only need the GOIs
			tmp = reduceTo(x, what='row', to=  gene.cluster.order ) #function definition in file 'reduceTo.R'
			## we also only need the samples that have been selected:
			#tmp = reduceTo(tmp, what='col', to=  colnames(x@data)[ #function definition in file 'reduceTo.R'
			#				which(!is.na(x@userGroups[,x@usedObj$lastGroup])) ] )

			try ( write_as_sqlite3( tmp, paste(sep=".", outfile, 'sqlite3') ) ) #function definition in file 'ExportFunctions.R'
	}

}