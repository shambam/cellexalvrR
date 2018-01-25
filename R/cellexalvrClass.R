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
						usedObj=NULL,
						colors=NULL,
						groupSelectedFrom=NULL,
						mds=NULL,
						index=NULL,
						tfs=NULL,
						version=NULL,
						print = function (...) {
							cat (paste("An object of class", paste(collapse="::",rev(class(self))),"\n" ) )
							#cat("named ",self$name,"\n")
							cat (paste( 'with',nrow(self$data),'genes and', ncol(self$data),' samples.'),"\n")
							cat (paste("Annotation datasets (",paste(dim(self$meta.gene),collapse=','),"): '",paste( colnames(self$meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
							cat (paste("Sample annotation (",paste(dim(self$meta.cell),collapse=','),"): '",paste( colnames(self$meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
							cat ( paste("Up to now I have",(ncol(self$userGroups)/2), "user groups stored" ),"\n")
							if ( length(names(self$mds)) > 0 ){
								cat ( "and ", length(names(self$mds)), " mds object(s)\n")
								cat ( "named ", paste(sep=', ', names(self$mds)), "\n")
							}
						},
						initialize = function (dat,meta.cell, meta.gene=NULL, mds=list(),index = NULL  ){
							
							self$data <- dat
							self$meta.gene <- meta.gene
							
							if (is.null(meta.gene) | ncol(meta.gene) == 0 ) {
								self$meta.gene <- matrix(ncol=1, c(rownames(dat) ) )
								colnames(self$meta.gene) = c('Gene Symbol')
								rownames(self$meta.gene) = rownames(dat)
							}
							
							self$meta.cell <- meta.cell
							self$userGroups <- data.frame()
							self$colors <- list()
							self$groupSelectedFrom <- NULL
							self$mds <- mds
							self$index = index
							self$force.numeric()
							self$version = sessionInfo('cellexalvr')$otherPkgs[[1]]$Version
							self
						},
						force.numeric = function(...) {
							storage.mode(self$data) <- 'numeric'
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
			## would the updated logging be good here??
			logErr <- function() {
				# turn logging callback off while we process errors separately
				tcbm$suspend(TRUE)
				# turn them back on when we're done
				on.exit(tcbm$suspend(FALSE))
				sc <- sys.calls()
				sclen <- length(sc)  # last call is this function call
				if(sclen > 1L) {
					cat("myError:\n", do.call(paste, c(lapply(sc[-sclen], deparse), sep="\n")), "\n", file="logfile.R", append=T)
				} else {
					# syntax error, so no call stack
					# show the last line entered
					# (this won't be helpful if it's a parse error in a function)
					file1 <- tempfile("Rrawhist")
					savehistory(file1)
					rawhist <- readLines(file1)
					unlink(file1)
					cat("myError:\n", rawhist[length(rawhist)], "\n", file="logfile.R", append=T)
				}
			}
			options(error=logErr)
# top-level callback handler
			log <- function(expr, value, ok, visible) {
				cat(deparse(expr), "\n", file="logfile.R", append=T)
				TRUE
			}
			tcbm <- taskCallbackManager()
			tcbm$add(log, name = "log")
#	r6x::formalizeClasses()
		}
		
