#' convert a list of genes into a collapsed markdown text that the user can expand.
#'
#' @name md_gene_links
#' @aliases md_gene_links,cellexalvrR-method
#' @rdname md_gene_links-methods
#' @docType methods
#' @description Bin the UMI data into 13 bins for plotting and define a blue <- red color gradient
#' @param x the cellexalvrR object
#' @param group the group (defaule = 'nUMI'
#' @param where in the samples (sample) or annotation (gene) data frame (sample)
#' @param colFun colory function default=  gplots::bluered
#' @title Create a binned annotation column from numeric data
#' @export 
setGeneric('md_gene_links', ## Name
	function ( genes ) { 
		standardGeneric('md_gene_links')
	}
)

setMethod('md_gene_links', signature = c ('character'),
	definition = function ( genes ) {

		text = paste( collapse=" ", sep=" ",
		 	paste( sep=" ", unlist( lapply(genes, function(n) { 
		 		rmdLink(n, "https://www.genecards.org/cgi-bin/carddisp.pl?gene=", lineEnd=FALSE )  })) 
		 	) ,
		"\n")

		## https://gist.github.com/pierrejoubert73/902cc94d79424356a8d20be2b382e1ab
		text = paste(sep="\r\n", "<details>", "  <summary>Click to expand gene list</summary>", "", text,"</details>","","")
		text
	}
)