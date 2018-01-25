#' @name get.genes.cor.to
#' @aliases get.genes.cor.to,cellexalvr-method
#' @rdname get.genes.cor.to-methods
#' @docType methods
#' @description VR function that exports all genes correlating to the input gene name
#' @param cellexalObj the cellexal object
#' @param gname the gene name of interst
#' @param output the VR outpath
#' @title description of function get.genes.cor.to
#' @export 
if ( ! isGeneric('get.genes.cor.to') ){ setGeneric('get.genes.cor.to', ## Name
			function (cellexalObj,gname,output) { ## Argumente der generischen Funktion
				standardGeneric('get.genes.cor.to') ## der Aufruf von standardGeneric sorgt für das Dispatching
			}
	)
}else {
	print ("Onload warn generic function 'get.genes.cor.to' already defined - no overloading here!")
}

setMethod('get.genes.cor.to', signature = c ('character'),
		definition = function (cellexalObj,gname,output) {
			cellexalObj <- loadObject(cellexalObj)
			get.genes.cor.to( cellexalObj,gname,output )
		}
)

setMethod('get.genes.cor.to', signature = c ('cellexalvr'),
		definition = function (cellexalObj,gname,output) {		
			cellexalObj <- loadObject(cellexalObj)
			dat <- cellexalObj$data
			rownames(dat) <- tolower(rownames(dat))
			
			goi <- dat[gname,]
			
			calc.cor <- function(v,comp){
				cor(v,comp)
			}
			
			cor.values <- apply(dat,1,calc.cor,comp=goi)
			
			ord <- names(sort(cor.values))
			
			pos <- ord[ (length(ord)-1): (length(ord)-10) ]
			neg <- ord[1:10]
			tab <- cbind(pos,neg)
			
			write.table(t(tab),output,row.names=F,col.names=F,sep="\t",quote=F)
		} 
)
