#' This function is part of the conversion path of a BioData::R6 object 
#' into a cellexalvrR object.
#'
#' The function will most likely not be of importance to anybody but me.
#' @name as_cellexalvrR
#' @aliases as_cellexalvrR,environment-method
#' @rdname as_cellexalvrR-methods
#' @docType methods
#' @description convert a BioData list (BioData library not loaded) into a cellexalvrR obejct
#' @param x the BioData 'object'
#' @param meta.cell.groups which x$samples column to convert to meta.cell classes
#' @param meta.genes.groups which annotation columns to keep (default NULL)
#' @param userGroups which x$samples columns to add to the userGroups slot
#' @param outpath set the outpath of the object (default getwd())
#' @param specie set the specie to either mouse or human (default check gene names)
#' @title convert a BioData object to cellexalvrR keeping all 3D drc objects.
#' @export 
setGeneric('as_cellexalvrR', ## Name
	function ( x, meta.cell.groups, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) { ## Argumente der generischen Funktion
		standardGeneric('as_cellexalvrR') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('as_cellexalvrR', signature = c ('environment'),
	definition = function ( x, meta.cell.groups, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
	## x has to be a BioData object which is read as a simple list here!
	ret = methods::new('cellexalvrR')
	ret@data = x$zscored
	#ret@data@x = log( exp( ret@data@x ) +1 ) ## fixed in BioData
	
	if ( ! is.null(meta.genes.groups) )
		ret@meta.gene = x$annoatation[, meta.genes.groups]
	ret@meta.cell = make.cell.meta.from.df( x$samples[,meta.cell.groups] ,rq.fields= meta.cell.groups )
	rownames(ret@meta.cell) = colnames( ret@data )
	t = data.frame(lapply( 
		x$usedObj$userGroups, 
		function(n) {
			OK = which(! is.na( x$samples[,n]))
			order=as.vector(x$samples[,n])
			order[OK] = 1:length(OK)
			list( x$samples[,n],order) 
		} ))
	
	colnames(t) = unlist(lapply( x$usedObj$userGroups, function (n) paste( n, c("", "order"))))
	ret@userGroups = t
	
	DRC <- names(x$usedObj)[grep ( 'MDS', names(x$usedObj))]
	OK = grep ( '_dim_' , DRC, invert= TRUE )
	if ( length(OK) == 0 ) {
		stop( "cellexalvrR does need at least one 3D DRC structure to work on - please create that first!")
	}
	for ( n in DRC[OK] ) {
		for ( n2 in names(x$usedObj[[n]]) ) {
			new_name = stringr::str_replace_all( n2, "\\s", "_")
			ret@drc[[new_name]] = x$usedObj[[n]][[n2]]
		}
	}
	ret@colors = x$usedObj$colorRange
	ret@specie=x$usedObj$specie
	
	bad = which( ret@data@x < 0)
	if ( length(bad) > 0 ) {
		ret@data@x[ bad ] = 0
		ret@data = Matrix::drop0(ret@data)
	}
	ret@outpath = outpath
	ret
} )

setMethod('as_cellexalvrR', signature = c ('character'),
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
	
	if (!file.exists(x)) {
		stop("sorry, x is not file I can read from")
	}
	
	if ( length(grep('.h5ad$',x)) ==1 ) {
		x= hdf5r::h5file(x)
		as_cellexalvrR( x, meta.cell.groups=meta.cell.groups, 
			meta.genes.groups = meta.genes.groups, userGroups= userGroups, outpath=outpath, 
			specie = specie)
	}else if( length(grep('.loom$',x)) ==1 ) {
		x <- loomR::connect(filename = x, mode = "r")
		as_cellexalvrR( x, meta.cell.groups=meta.cell.groups, 
			meta.genes.groups = meta.genes.groups, userGroups= userGroups, outpath=outpath, 
			specie = specie)
	}
	else {
		stop(paste("Sorry the file",x,"is not supported by this function" ) )
	}
} )

setMethod('as_cellexalvrR', signature = c ('loom'),
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
		## code adapted from Seurat::ReadH5AD.H5File() 2019/08/22
	
	require( 'hdf5r' )

	ret = methods::new('cellexalvrR')
	ret@specie = specie

	as.sparse <- function(x, ...) {
  for (i in c('data', 'indices', 'indptr')) {
    if (!x$exists(name = i) || !is(object = x[[i]], class2 = 'H5D')) {
      stop("Invalid H5Group specification for a sparse matrix, missing dataset ", i)
    }
  }
  if ('h5sparse_shape' %in% hdf5r::h5attr_names(x = x)) {
    return(Matrix::sparseMatrix(
      i = x[['indices']][] + 1,
      p = x[['indptr']][],
      x = x[['data']][],
      dims = rev(x = hdf5r::h5attr(x = x, which = 'h5sparse_shape'))
    ))
  }
  return(Matrix::sparseMatrix(
    i = x[['indices']][] + 1,
    p = x[['indptr']][],
    x = x[['data']][],
    sparse=T
  ))
	}
  	if (is(object = x[['matrix']], class2 = 'H5Group')) {
    	dat <- as.sparse(x = x[['matrix']])
  	} else {
   		dat <- x[['matrix']][, ]
  	}
  	# x will be an S3 matrix if X was scaled, otherwise will be a dgCMatrix
  	if (is.matrix(x = dat)) {
  		## the loom files seam to store all data as matrix and not as sparse matrix?!
  		dat = Matrix::Matrix(dat, sparse=T)
  	}

  	toDF <- function( slotName, namecol ) {
  		obs = data.frame(lapply(names(x[[slotName]]), function(n) { x[[paste(sep="/",slotName,n)]][] } ))
  		colnames( obs ) = names(x[[slotName]])
  		col_uniq= NULL
  		for( n in colnames(obs) ) {
	  		if ( all(obs[,n] =="") ){
  				obs[,n] = NULL
  			}else {
	  			suppressWarnings({OK = length(which( is.na(as.numeric(obs[,n])) )) })
  				col_uniq = c( col_uniq, OK )
  			}
  		}
  		## most likely cell names column
  		if ( ! is.na(match(namecol, colnames(obs)) )) {
			rownames(obs) =  forceAbsoluteUniqueSample (
				as.vector(obs[, namecol]) )
  		}else {
	  		rownames(obs) =  forceAbsoluteUniqueSample (
  				as.vector(obs[, which(col_uniq == max(col_uniq))[1]]))
  		}
  		obs
  	}
  	obs = toDF('col_attrs', 'cell_names' )
 	#obs = data.frame(lapply(names(x[['col_attrs']]), function(n) { x[[paste(sep="/",'col_attrs',n)]][] } ))
  	#colnames( obs ) = names(x[['col_attrs']])
  	#col_uniq= NULL
  	#for( n in colnames(obs) ) {
  	#	if ( all(obs[,n] =="") ){
  	#		obs[,n] = NULL
  	#	}else {
  	#		suppressWarnings({OK = length(which( is.na(as.numeric(meta.features[,n])) )) })
  	#		col_uniq = c( col_uniq, OK )
  	#	}
  	#}
  	## most likely cell names column
  	#if ( ! is.na(match('cell_names', colnames(obs)) )) {
	#	rownames(obs) =  forceAbsoluteUniqueSample (
	#		as.vector(obs[, 'cell_names']) )
  	#}else {
  	#	rownames(obs) =  forceAbsoluteUniqueSample (
  	#		as.vector(obs[, which(col_uniq == max(col_uniq))[1]]))
  	#}

  	meta.features = toDF('row_attrs', 'gene_names' )
  	#meta.features = data.frame(lapply(names(x[['row_attrs']]), function(n) { x[[paste(sep="/",'row_attrs',n)]][] } ))
  	#colnames( meta.features ) = names(x[['row_attrs']])
  	#row_uniq= NULL
  	#for( n in colnames(meta.features) ) {
  	#	if ( all(meta.features[,n] =="") ){
  	#		meta.features[,n] = NULL
  	#	}else{
  	#		suppressWarnings({OK = length(which( is.na(as.numeric(meta.features[,n])) )) })
  	#		row_uniq = c( row_uniq, OK )
  	#	}
  	#}
  	## most likely gene names column
  	#if ( ! is.na(match('gene_names', colnames(meta.features)) )) {
	#	rownames(meta.features) =  forceAbsoluteUniqueSample (
	#		as.vector(meta.features[, 'gene_names']) )
  	#}else {
  	#	rownames(meta.features) =  forceAbsoluteUniqueSample (
  	#		as.vector(meta.features[, which(row_uniq == max(row_uniq))[1]]))
  	#}
  	#browser()
  	dat = Matrix::t(dat)
  	rownames(dat) = rownames( meta.features)
  	colnames(dat) = rownames(obs)
	ret@data = dat

	ret = addCellMeta2cellexalvr(ret, obs)
	ret@meta.gene = as.matrix(meta.features[match( rownames(ret@data), rownames(meta.features) ),])

	rm( dat)
	rm( meta.features)
	rm(obs)

	## The drc's are hidden in the obj
	interest <- list( 
		'unknown' = c('_X', '_Y', '_Z'), 
		'tSNE' = c('_tSNE1', '_tSNE2', '_tSNE3'), 
		'PCA' = c('_PC1', '_PC2', '_PC3') 
	)

	dr = lapply( interest, function(a) { 
		d=NULL
		
		if ( var(ret@meta.cell[,a[1]]) + var (ret@meta.cell[,a[2]]) != 0 ){
			## the third column is not defined in the loom structures. Hence I simply do not check for it here
			d= cbind(as.numeric(as.vector(ret@meta.cell[,a[1]])), as.numeric(as.vector(ret@meta.cell[,a[2]])), rep(0,nrow(ret@meta.cell)) )
			colnames(d) = a
			rownames(d) = rownames(ret@meta.cell)
		}
		d
		} )

	for( n in names(dr) ) {
		if ( ! is.null(dr[[n]])) {
			ret@drc[[n]] = dr[[n]]
		}
	}


    if ( length(names(ret@drc)) == 0) {
    	stop( "No usable drc's found in the loomR object")
    }

    ret@specie = specie
    ret@outpath = outpath
    #print ( "Please take care colnames and rownames have not been set!" )
    invisible(ret)
} )

setMethod('as_cellexalvrR', signature = c ('H5File'),
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
		## code adapted from Seurat::ReadH5AD.H5File() 2019/08/22
	
	require( 'hdf5r' )

	ret = methods::new('cellexalvrR')
	ret@specie = specie

	as.sparse <- function(x, ...) {
  for (i in c('data', 'indices', 'indptr')) {
    if (!x$exists(name = i) || !is(object = x[[i]], class2 = 'H5D')) {
      stop("Invalid H5Group specification for a sparse matrix, missing dataset ", i)
    }
  }
  if ('h5sparse_shape' %in% hdf5r::h5attr_names(x = x)) {
    return(sparseMatrix(
      i = x[['indices']][] + 1,
      p = x[['indptr']][],
      x = x[['data']][],
      dims = rev(x = hdf5r::h5attr(x = x, which = 'h5sparse_shape'))
    ))
  }
  return(sparseMatrix(
    i = x[['indices']][] + 1,
    p = x[['indptr']][],
    x = x[['data']][]
  ))
}
  	if (is(object = x[['X']], class2 = 'H5Group')) {
    	dat <- as.sparse(x = x[['X']])
  	} else {
   		dat <- x[['X']][, ]
  	}
  	# x will be an S3 matrix if X was scaled, otherwise will be a dgCMatrix
  	scaled <- is.matrix(x = dat)

	obs <- x[['obs']][]
  	dat.var <- x[['var']][]
  	rownames(x = dat) <- rownames(x = dat.var) <- dat.var$index
  	colnames(x = dat) <- rownames(x = obs) <- obs$index	
  	x.slot = FALSE
  	meta.features <- NULL
  	if ( scaled ){
  		if (x$exists(name = 'raw.X')) {
  			dat <- as.sparse(x = x[['raw.X']])
    		add.var <- x[['raw.var']][]
    		slot(object = dat, name = 'Dim') <- c(nrow(x = add.var), nrow(x = obs))
    		
    		rownames(x = dat) <- rownames(x = add.var) <- add.var$index
    		colnames(x = dat) <- obs$index
    		add.var <- add.var[, -which(x = colnames(x = add.var) == 'index'), drop = FALSE]
    		#Merging feature-level metadata dataframes
    		dat.var <- dat.var[, -which(x = colnames(x = dat.var) %in% colnames(x = add.var))]
   			meta.features <- merge(x = add.var, y = dat.var, by = 0, all = TRUE)
    		rownames(x = meta.features) <- meta.features$Row.names
    		meta.features <- meta.features[, -which(x = colnames(x = meta.features) == 'Row.names'), drop = FALSE]
    		rm(add.var)
    		rm ( dat.var)
  		}else {
  			stop( "The AnnData object does not contain unscaled data!")
  		}
  	}else {
  		meta.features <- dat.var
  		rm ( dat.var )
  	}

	## obtain the data slot information
	ret@data = dat

	ret = addCellMeta2cellexalvr(ret, obs)
	ret@meta.gene = as.matrix(meta.features[match( rownames(ret@data), rownames(meta.features) ),])

	rm( dat)
	rm( meta.features)
	rm(obs)

	if ( ! x$exists(name = 'obsm') ) {
		stop( "The AnnData does not contain any dimension reduction datasets needed for cellexalVR" )
	}
	# Pull cell embeddings
	embed.reduc <- x[['obsm']]$get_type()$get_cpd_labels()
	embed.n <- sapply(
      X = x[['obsm']]$get_type()$describe()$cpd_types,
      FUN = '[[',
      'array_dims'
    )
    names(x = embed.n) <- embed.reduc
    ncells <- x[['obsm']]$dims
    embeddings <- lapply(
      X = embed.reduc,
      FUN = function(r) {
        return(t(x = vapply(
          X = 1:ncells,
          FUN = function(i) {
            return(x[['obsm']][i][[r]])
          },
          FUN.VALUE = numeric(length = embed.n[[r]])
        )))
      }
    )
    names(x = embeddings) <- embed.reduc
    for (i in 1:length(x = embeddings)) {
      rownames(x = embeddings[[i]]) <- colnames(x = ret@data )
    }
    ## now add all >= 3D MDS objects to ret
    for ( name in names(embeddings)) {
    	if ( ncol(embeddings[[name]] ) >= 3 ){
    		## get rid of all dimensions > 3
    		ret@drc[[name]] = embeddings[[name]][,1:3] 
    	}else {
    		ret@drc[[name]] = cbind( embeddings[[name]], z= rep(0,nrow(embeddings[[name]])) )
    	}
    }
    if ( length(names(ret@drc)) == 0) {
    	stop( "No usable drc's found in the AnnData object")
    }

    ret@specie = specie
    ret@outpath = outpath

    invisible(ret)
} )

#' This function adds _<id> to all duplicate values thereby enforcing uniques.
forceAbsoluteUniqueSample = function( x ,separator='_') {
	ret <- vector(length=length(x))
	last = x[1]
	ret[1] <- last
	for ( i in 2:length(x) ){
		last = x[i]
		if ( ! is.na(match( last, ret )) ){
			last <- paste(last,separator,sum( ! is.na(match( x[1:i], last )))-1, sep = '')
		}
		ret[i] <- last
	}
	ret
}