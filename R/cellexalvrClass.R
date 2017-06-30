require('R6')
#'Class defintion of cellexalvr
#' @title cellexalvr
#' @docType class
#' @description  An R S4 class that represents the backend for the cellexal VR application
#' @field data a matrix storing the expression values
#' @field meta.cell a matrix storing the sample meta.gene data (data column)
#' @field meta.gene a matrix storing the gene meta.gene data (data row)
#' @field mds list of mds representations of the whole data frame
#' @field index a matrix of experimental sample descriptions e.g. FACS levels
#' @field tfs a vector of transcription factor names?
#' @field stats a list to harbor stat data tables. 
#' The stat tables need to have the same gene order as the data table
#' @field name the name of the object (mainly unused)
#' @field usedObj a list to store temporary data like color information and other loosely linked data
#' @export
cellexalvr <- R6Class(
		"cellexalvr",
		class = TRUE,
		public = list ( 
				data=NULL,
				name="cellexalvr",
				meta.cell=NULL,
				meta.gene=NULL,
				mds=list(),
				index=NULL,
				tfs=NULL,
				usedObj = list(),
				stats = list(),
				print = function (...) {
					cat (paste("An object of class", paste(collapse="::",rev(class(self))),"\n" ) )
					cat("named ",self$name,"\n")
					cat (paste( 'with',nrow(self$data),'genes and', ncol(self$data),' samples.'),"\n")
					cat (paste("meta.gene datasets (",paste(dim(self$meta.gene),collapse=','),"): '",paste( colnames(self$meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
					cat (paste("Sample meta.gene (",paste(dim(self$meta.cell),collapse=','),"): '",paste( colnames(self$meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
					if ( length(names(self$stats)) > 0 ){
						cat ( "P values were calculated for ", length(names(self$stats)), " condition(s)\n")
					}
				},
				initialize = function (dat,meta.cell, meta.gene, name='cellexalvr', namecol=NULL ){
					
					self$meta.cell <- data.frame(meta.cell)
					self$name <- name
					self$meta.gene <- data.frame(meta.gene)
					self$usedObj <- list()
					if ( is.null(namecol)){
						namecol = colnames(meta.cell)[1]
					}
					self$data <- dat
					colnames(self$data) <- make.names(self$forceAbsoluteUniqueSample ( as.vector(meta.cell[, namecol]) ))
					self$meta.cell[,'samples'] <- colnames(self$data)
				},
				pwd = function () {
					system( 'pwd > __pwd' )
					t <- read.delim( file = '__pwd', header=F)
					t <- as.vector(t[1,1])
					t <- paste(t,"/",sep='')
					unlink( '__pwd')
					t
				},
				forceAbsoluteUniqueSample = function( x ,separator='_') {
					last = ''
					ret <- vector(length=length(x))
					for ( i in 1:length(x) ){
						if ( is.null(ret) ){
							last = x[i]
							ret[i] <- last
						}
						else{
							last = x[i]
							if ( ! is.na(match( last, ret )) ){
								last <- paste(last,separator,sum( ! is.na(match( x[1:i], last )))-1, sep = '')
							}
							ret[i] <- last
						}
					}
					ret
				}
		)
)






.onAttach <- function(libname, pkgname) {
	#packageStartupMessage("Welcome to my package BioData")
	where <- as.environment("package:cellexalvr")
	clss <- list(
			c("cellexalvr", "R6")
	)
	## Ensure clean initial state for subsequent package loads
	## while developing //
	sapply(clss, function(cls) {
				idx <- sapply(cls, isClass )
				suppressWarnings(try(sapply(cls[idx], removeClass,
										where = where), silent = TRUE))
			})
	## Set formal class equivalent //
	sapply(clss, function(cls) {
				try(setOldClass(cls, where = where), silent = TRUE)
			})
#	r6x::formalizeClasses()
}


