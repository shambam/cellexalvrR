#' Cellexalvr, a class to interact with the VR environment developed by the Computational Genomics goup in Lund
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom grDevices dev.off png rainbow x11
#' @importFrom graphics legend axis barplot image par pie abline hist layout lines mtext plot plot.new rect text title
#' @importFrom stats quantile as.dendrogram density dist hclust median order.dendrogram reorder sd
#' @export
#' @keywords cellexalvr
#' @return Object of \code{\link{R6Class}} to interact with the VR environment.
#' @format \code{\link{R6Class}} object.
#' @examples
#' set.seed(1)
#' dat =  matrix(rnorm(1000),ncol=10) 
#' colnames(dat) <- paste('Sample', 1:10)
#' rownames(dat) <- paste( 'gene', 1:100)
#' samples <- data.frame(SampleID = 1:10, sname = colnames(dat) )
#' annotation <- data.frame( GeneID = paste( 'gene', 1:100), Start= 101:200 )
#' x <- cellexalvr( data=dat, meta.gene=annotation, meta.cell = samples )
#' @field data the numerical data as data.frame
#' @field meta.cell the sample annotation as data.frame
#' @field meta.gene the row annotation as data.frame
#' @field usedObj a multi purpose list to store whichever ananlyis results do not fit in the stats list
#' @export 

cellexalvr <- #withFormalClass(
		R6Class(
				'cellexalvr',
				class = TRUE,
				public = list ( 
						data=NULL,
						meta.cell=NULL,
						meta.gene=NULL,
						userGroups=NULL,
						colors=NULL,
						groupSelectedFrom=NULL,
						mds=NULL,
						index=NULL,
						tfs=NULL,
						print = function (...) {
							cat (paste("An object of class", paste(collapse="::",rev(class(self))),"\n" ) )
							#cat("named ",self$name,"\n")
							cat (paste( 'with',nrow(self$data),'genes and', ncol(self$data),' samples.'),"\n")
							cat (paste("Annotation datasets (",paste(dim(self$meta.gene),collapse=','),"): '",paste( colnames(self$annotation ), collapse="', '"),"'  ",sep='' ),"\n")
							cat (paste("Sample annotation (",paste(dim(self$meta.cell),collapse=','),"): '",paste( colnames(self$samples ), collapse="', '"),"'  ",sep='' ),"\n")
							cat ( paste("Up to now I have",(ncol(self$userGroups)/2), "user groups stored" ),"\n")
							if ( length(names(self$mds)) > 0 ){
								cat ( "and ", length(names(self$mds)), " mds object(s)\n")
								cat ( "named ", paste(sep=', ', names(self$mds)), "\n")
							}
						},
						initialize = function (dat,meta.cell, meta.gene=NULL, mds=list()  ){
							
							self$data <- dat
							self$meta.gene <- meta.gene
							
							if (is.null(meta.gene) ) {
								self$meta.gene <- matrix(ncol=1, c(rownames(dat) ) )
								colnames(self$meta.gene) = c('Gene Symbol')
								rownames(self$meta.gene) = rownames(dat)
							}
							
							self$meta.cell <- meta.cell
							self$userGroups <- data.frame()
							self$colors <- list()
							self$groupSelectedFrom <- NULL
							self$mds <- mds
							
							S <- Samples
							
							if ( is.null(namecol)){
								stop("Please specify the name of the sample name column (namecol)")
							}
							n <- make.names(as.vector(S[,namecol]))
							mat <- match( as.vector(S[,namecol]), colnames(dat))
							if ( sum(is.na(mat)) > 0 ) {
								stop(paste( 'The samples', 
												paste( as.vector(S[,namecol])[is.na(mat)], collapse=', '),
												'Do not have a data column in the "dat" data.frame' ) 
								)
							}
							
							self$data =  dat[, mat ]
							annotation <- dat[, is.na(match( colnames(dat), as.vector(S[,namecol]) ))==T ]
							
							if ( class(annotation) == 'factor'){
								annotation <- data.frame( annotation )
								colnames(annotation) <- namerow
							}
							if ( class(annotation) == 'character'){
								annotation <- data.frame( annotation )
								colnames(annotation) <- namerow
							}
							
							if ( outpath == '' ){
								outpath = self$pwd()
							}
							if ( ! file.exists(outpath)){
								dir.create( outpath )
							}
							
							if ( is.null(dim(annotation))){
								## this xcould be a problem... hope we at least have a vector
								if ( length(annotation) == nrow(self$data)) {
									rownames(self$data) <- annotation
								}
								else {
									stop ( "Sorry, please cbind the rownames to the expression values before creating this object!")
								}
							}else{
								rownames(self$data) <- annotation[,namerow]
							}
							self$samples <- S
							self$name <- name
							self$annotation <- annotation
							self$rownamescol <- namerow
							self$outpath <- outpath
							self$usedObj <- list()
							colnames(self$data) <- make.names(self$forceAbsoluteUniqueSample ( as.vector(S[, namecol]) ))
							self$samples$samples <- colnames(self$data)
							
							self$sampleNamesCol <- namecol
							self$force.numeric
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
			where <- as.environment("package:BioData")
			clss <- list(
					c("cellexalvr", "R6"),
				#	c("tRNAMINT", "BioData"),
				#	c("MicroArray", "BioData")
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
		
