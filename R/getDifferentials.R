
if ( ! isGeneric('getDifferentials') ){setGeneric('getDifferentials', ## Name
			function (cellexalObj,cellidfile,
					deg.method=c('wilcox', 'Seurat_wilcox', 'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova'),
					num.sig=250, Log=TRUE, logfc.threshold = 1, minPct=0.1, onlyPos=TRUE) { 
				standardGeneric('getDifferentials') 
			}
	) 
}

#' Identify differentially expressed genes.
#' 
#' This function makes three statistics available for the VR process 
#' (1) 'linear' correlation for one group selections
#' (2) 'wilcox' a c++ re-implementation of the Seurat::FindAllMarkers function
#' (3) 'Seurat_wilcox' the original Seurat::FindAllMarkers function (~10x slower than the c++ version)
#' @name getDifferentials
#' @aliases getDifferentials,cellexalvrR-method
#' @rdname getDifferentials-methods
#' @docType methods
#' @description  Creates a heatmap from a selection of groups
#' The Seurat based statsictsics is applied only to genes expressed in at least 1 percent of the cells.
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param deg.method The method to use to find DEGs ( 'wilcox', 'Seurat wilcox', 'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2' )
#' @param num.sig number of differnetial genes to return (250)
#' @param Log log the results (default=TRUE)
#' @param logfc.threshold the Seurat logfc.threshold option (default here 1 vs 0.25 in Seurat)
#' @param minPct the minium percent expressing cells in a group (default 0.1)
#' @param onlyPos select only genes showing an higher expression in the group (default =T)
#' @keywords DEGs
#' @title VR helper function getDifferentials
#' @examples 
#' \dontrun{
#' getDifferentials( cellexalObj,  cellidfile= 'User.group.2', deg.method='wilcox') #function definition in file 'getDifferentials.R'
#' }
#' @return a list of the top differential genes (num.sig)
#' @export getDifferentials
setMethod('getDifferentials', signature = c ('cellexalvrR'),
		definition = function (cellexalObj,cellidfile,
				deg.method=c('wilcox','Seurat_wilcox',  'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova'),
				num.sig=250, Log=TRUE, logfc.threshold = 1, minPct=0.1, onlyPos=TRUE) {
			
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			num.sig <- as.numeric( num.sig )
			
			accepted = c('wilcox','Seurat_wilcox',  'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova')
			if ( sum(unlist(lapply( accepted, function(ok) { return ( ok == deg.method )} ))) != 1 ) {
				stop( paste('The deg.method',deg.method, 'is not supported' ) )
			}
			cellexalObj <- userGrouping(cellexalObj, cellidfile) #function definition in file 'userGrouping.R'
			not <- which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
			if ( length(not) > 0) {
				loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj@data)[- not ] ) #function definition in file 'reduceTo.R'
			}else {
				loc <- cellexalObj
			}
			if ( ! is.na(match(paste(cellexalObj@usedObj$lastGroup, 'order'), colnames(cellexalObj@data))) ){
				loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order')) #function definition in file 'reorder.obj.R'
			}
			
			info <- groupingInfo( loc ) #function definition in file 'groupingInfo.R'
			
			rem.ind <- which(Matrix::rowSums(loc@data)==0)
			
			grp.vec <- info$grouping
			
			col.tab <- info$col
			
			if(length(rem.ind)>0){
				loc = reduceTo(loc, what='row', to=rownames(loc@data)[-rem.ind]) #function definition in file 'reduceTo.R'
			}
			
			deg.genes <- NULL
			if ( is.null(cellexalObj@usedObj$sigGeneLists)) 
				cellexalObj@usedObj$sigGeneLists = list()
			
			if(deg.method=='anova'){
				message('anova gene stats is deprecated - using wilcox instead!')
				deg.method= 'wilcox'
			}
			
			if(length(col.tab) == 1){
				deg.method == 'Linear'
				message('cor.stat linear gene stats')
				lin <- function( v, order ) {
					stats::cor.test( v, order, method='spearman' )
				}
				ps <- apply(loc@data,1,lin,order=1:ncol(loc@data))
				
				ps = data.frame((lapply(ps, function(x){ c(x$statistic, x$p.value) })))
				ps = data.frame(t(ps))
				colnames(ps) = c('statsistics', 'p.value' )
				sigp <- order(ps$p.value)[1:num.sig]
				deg.genes <- rownames(ps)[sigp]		
				
				ps[,'p.adj.fdr'] = stats::p.adjust(ps[,'p.value'], method = 'fdr')
				cellexalObj@usedObj$sigGeneLists$lin[[cellexalObj@usedObj$lastGroup]] = ps
				if ( Log ) {
					logStatResult( cellexalObj, 'linear', ps, 'p.adj.fdr' ) #function definition in file 'logStatResult.R'
				}
				
			}else if ( deg.method == 'wilcox') {
				## use the faster Rcpp implementation
				
				CppStats <- function( n ) {
					OK = which(grp.vec == n )
					BAD= which(grp.vec != n )
					r = as.data.frame(
							FastWilcoxTest::StatTest( Matrix::t( loc@data), OK, BAD, 
									logfc.threshold, minPct, onlyPos=onlyPos )
					)
					r= r[order( r[,'p.value']),]
					r = cbind( r, cluster= rep(n,nrow(r) ), gene=rownames(loc@data)[r[,1]] )
					r
				}
				
				all_markers = NULL;
				for ( n in  unique( sort(grp.vec)) ) {
					all_markers = rbind( all_markers, CppStats(n) )
				}
				
				#all_markers <- all_markers[ order( all_markers[,'p.value']),]
				if ( Log ) {
					logStatResult( cellexalObj, 'Cpp', all_markers, 'p.value' ) #function definition in file 'logStatResult.R'
				}
				if ( is.null(cellexalObj@usedObj$sigGeneLists$Cpp)) 
					cellexalObj@usedObj$sigGeneLists$Cpp = list()
				cellexalObj@usedObj$sigGeneLists$Cpp[[cellexalObj@usedObj$lastGroup]] = all_markers
			}
			# else {
			# 	if ( deg.method == 'Seurat_wilcox') {
			# 		deg.method = 'wilcox'
			# 	} 
			# 	message(paste('Seurat::FindAllMarkers gene stats using stat method',deg.method)  )
			# 	## in parts copied from my BioData::createStats() function for R6::BioData::SingleCells
				
			# 	if (!requireNamespace('Seurat', quietly = TRUE)) {
			# 		stop('seurat needed for this function to work. Please install it.',
			# 				call. = FALSE)
			# 	}
			# 	sca <- Seurat::CreateSeuratObject(loc@data, project = 'SeuratProject', min.cells = 0,
			# 			min.genes = ceiling(ncol(loc@data)/100), is.expr = 1, normalization.method = NULL,
			# 			scale.factor = 10000, do.scale = FALSE, do.center = FALSE,
			# 			names.field = 1, names.delim = '_', 
			# 			meta.data = data.frame(wellKey=colnames(loc@data), GroupName = grp.vec),
			# 			display.progress = TRUE)
				
			# 	sca = Seurat::SetIdent( sca, colnames(loc@data), 
			# 			paste('Group', as.character(loc@userGroups[ ,cellexalObj@usedObj$lastGroup]) ) )
			# 	all_markers <- Seurat::FindAllMarkers(
			# 			object = sca, test.use = deg.method, logfc.threshold = logfc.threshold, minPct=minPct , only.pos=onlyPos
			# 	)
			# 	if ( Log ) {
			# 		logStatResult( cellexalObj, 'Seurat', all_markers, 'p_val_adj' ) #function definition in file 'logStatResult.R'
			# 	}
			# 	if ( is.null(cellexalObj@usedObj$sigGeneLists$Seurat)) 
			# 		cellexalObj@usedObj$sigGeneLists$Seurat = list()
			# 	cellexalObj@usedObj$sigGeneLists$Seurat[[cellexalObj@usedObj$lastGroup]] = all_markers
			# }
			else {
				stop(paste('The stats method', deg.method, "is not supported by this version of cellexalvrR"))
			}
			
			### get the top genes
			if ( deg.method != 'Linear' ) {
				genes_list <- split( as.vector(all_markers[,'gene']), all_markers[,'cluster'] )
				ret_genes =  ceiling(num.sig / length(table(grp.vec)))
				
				if ( ret_genes < 1)
					ret_genes = 1
				
				top_genes <- function( x ) {
					if ( length(x) == 0) {
						NA
					}
					else if ( length(x) < ret_genes ) {
						x
					}else {
						x[1:ret_genes]
					}
				}
				
				## likely not the best approach..
				deg.genes = NULL
				ret_genes = ret_genes -1
				i = 0
				while ( length( deg.genes ) < num.sig ) {
					ret_genes = ret_genes +1
					i = i+1
					deg.genes = unique(unlist( lapply( genes_list,top_genes ) ))
					bad = which(is.na(deg.genes))
					if ( length(bad) > 0) 
						deg.genes = deg.genes[-bad]
					if ( i > 20)
						break
				}
				
				deg.genes = rownames(cellexalObj@data)[ match( make.names(deg.genes), make.names( rownames( cellexalObj@data) ) )]
				loc = reduceTo(loc, what='row', to=deg.genes) #function definition in file 'reduceTo.R'
				#tab <- as.matrix(Matrix::t(loc@data))
				if ( length(which(is.na( loc@userGroups[, loc@usedObj$lastGroup]) )) > 0 ) {
					## shit that will not work!
					loc = reduceTo(loc, what='col', to= which(is.na( loc@userGroups[, cellexalObj@usedObj$lastGroup]) ==F) ) #function definition in file 'reduceTo.R'
				}
				
				tab <- t(FastWilcoxTest::collapse( loc@data, as.numeric(factor( as.vector(loc@userGroups[, loc@usedObj$lastGroup]) ) ), 1 )) ## simple sum up the data
				tab[which(tab == -Inf)] = 0
				hc <- stats::hclust(stats::as.dist( 1- stats::cor(tab, method='pearson') ),method = 'ward.D2')
				deg.genes = rownames(loc@data)[hc$order]
			}
			
			if ( length(deg.genes) == 0){
				message('deg.genes no entries - fix that')
				if ( interactive() ) {
					message ( 'no signififcant genes detected! - help needed: (exit with Q)' )
					browser()
				}else {
					message ( 'no signififcant genes detected!' )
				}
				
			}
			#promise <- future(lockedSave(cellexalObj), evaluator = plan('multiprocess') ) #function definition in file 'lockedSave.R'
			## we only need to store the stats object here.
			## and as that is part of the usedObj we will store that ;-)
			## lockedSave(cellexalObj) ## to much overheard! #function definition in file 'lockedSave.R'
			if ( ! interactive() ) { ## likely the VR scripts
				#print( paste('Do we reach this point?', 'usedObj', cellexalObj@outpath ) )
				savePart( cellexalObj, 'usedObj'); #function definition in file 'integrateParts.R'
				#print( 'And this - Do we reach this point, too?')
			}
			
			deg.genes
		}
)


#' @describeIn getDifferentials cellexalvrR
#' @docType methods
#' @description preload the cellexalObj
#' @param cellexalObj the cellexal.RData file
#' @param cellidfile file containing cell IDs
#' @param deg.method The method to use to find DEGs ( 'wilcox', 'Seurat wilcox', 'bimod',
#' 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2')
#' @param num.sig number of differnetial genes to return (250)
#' @param Log log the results (default=TRUE)
#' @param logfc.threshold the Seurat logfc.threshold option (default here 1 vs 0.25 in Seurat)
#' @param minPct the minium percent expressing cells in a group (default 0.1)
#' @keywords DEGs
#' @title description of function getDifferentials
#' @export getDifferentials
setMethod('getDifferentials', signature = c ('character'),
		definition = function (cellexalObj,cellidfile,
				deg.method=c('wilcox', 'Seurat_wilcox', 'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova'),
				num.sig=250, Log=TRUE, logfc.threshold = 1, minPct=0.1) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			getDifferentials( cellexalObj,cellidfile,deg.method,num.sig, Log=Log) #function definition in file 'getDifferentials.R'
		}
)
