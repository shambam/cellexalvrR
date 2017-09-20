#' @name as_BioData
#' @rdname as_BioData-methods
#' @docType methods
#' @description create a BioData object from a cellexalvr object
#' @param dat the cellexalvr object
#' @title description of function as
#' @export 
as_BioData = function ( dat ) {
			#cbind(annotation,dat), Samples=samples, name="testObject",namecol='sname', outpath = ""
			ok = which(lapply(colnames(dat@meta.cell) , function(x) { all.equal( as.character(as.vector(dat@meta.cell[,x])), colnames(dat@data)) == T } )==T)
			if ( length(ok) == 0) {
				if (all.equal( rownames(dat@meta.cell), colnames(dat@data)) ){
					dat@meta.cell = cbind( cell.name = colnames(dat@data), dat@meta.cell)
					namecol = 'cell.name'
				}
			}
			else {
				namecol = colnames(dat@meta.cell)[ok]
				namecol = namecol[1]
			}
			namerow = NULL
			if (nrow(dat@meta.gene)==0) {
				dat@meta.gene <- matrix(ncol=2, c(rownames(dat@data), rep( 0, nrow(dat@data)) ) )
				colnames(x@meta.gene) = c('Gene.Symbol', 'useless')
				rownames(x@meta.gene) = rownames(x@data)
				namerow = 'Gene.Symbol'
			}else {
				ok = which(lapply(colnames(dat@meta.gene) , function(x) { all.equal( as.character(as.vector(dat@meta.gene[,x])), rownames(dat@data)) == T } )==T)
				if ( length(ok) == 0) {
					if (all.equal( rownames(dat@meta.gene), rownames(dat@data)) ){
						dat@meta.gene = cbind( gene.name = rownames(dat@data), dat@meta.gene)
						namerow = 'gene.name'
					}
				}
				else {
					namerow = colnames(dat@meta.gene)[ok]
					namerow = make.names(namerow[1])
				}
			}
			storage.mode(dat@data) <- 'numeric'
			d <- data.frame(cbind( dat@meta.gene, dat@data))
			ret <- BioData$new( d, Samples=data.frame(cbind(dat@meta.cell, dat@userGroups)), name= 'from.cellexalvr', namecol= namecol, namerow=namerow, outpath='./' )
			ret$usedObj <- dat@usedObj
			ret
		} 