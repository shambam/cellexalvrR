#' Calculates a simple entropy value for a grouping vector and a 3D drc data.
#'
#' @name NaiveEntropy
#' @aliases NaiveEntropy,cellexalvrR-method
#' @rdname NaiveEntropy-methods
#' @docType methods
#' @description 
#' @param x the cellexalvrR object
#' @param gvect a vector with group ids
#' @param drc the dimension reduction name to process
#' @param n  the amount of nearest neighbors (default= 20)
#' @param sumFunc function to sum up the single entropy values (default sum)
#' @title description of function NaiveEntropy
#' @export 
setGeneric('NaiveEntropy', ## Name
	function (x, gvect, drc, n = 20, sumFunc = sum()) { ## Argumente der generischen Funktion
		standardGeneric('NaiveEntropy') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('NaiveEntropy', signature = c ('cellexalvrR'),
	definition = function (x, gvect, drc, n = 20, sumFunc = sum ) {
        ## this will calculate for each entry the eucledian distance 
        ## in the 3D space to all others and get the n closest ids,
        ## checks out the groupID of the colsest cells and calculate the entropy
        if ( !is.function(sumFunc) ){
                stop( "sumFunc needs to be an R function" )
        }
        pb <- progress::progress_bar$new(total = length(gvect))
        closest= data.frame(lapply( 1:length(gvect), function( id ) {
                d = FastWilcoxTest::eDist3d( x@drc[[drc]][,1], x@drc[[drc]][,2],x@drc[[drc]][,3], id -1 )
#               order(d)[1:n]
                pb$tick()
                gvect[order(d)[1:n]]
        }))
        pb <- progress::progress_bar$new(total = length(gvect))
        entropy = apply( closest,2, function(d) {
                a = table(d)
                ret = 0
                if ( length(a) > 1 ) {
                        p = a/sum(a)
                        ret = -sum(p*log(p))
                }
                pb$tick()
                ret}
        )
        sumFunc(entropy)
} )

setMethod('NaiveEntropy', signature = c ('matrix'),
        definition = function (x, gvect, drc='unused', n = 20, sumFunc = sum ) {
        ## this will calculate for each entry the eucledian distance 
        ## in the 3D space to all others and get the n closest ids,
        ## checks out the groupID of the colsest cells and calculate the entropy
        if ( !is.function(sumFunc) ){
                stop( "sumFunc needs to be an R function" )
        }
        pb <- progress::progress_bar$new(total = length(gvect))
        closest= data.frame(lapply( 1:length(gvect), function( id ) {
                d = FastWilcoxTest::eDist3d( x[,1], x[,2],x[,3], id -1 )
#               order(d)[1:n]
                pb$tick()
                gvect[order(d)[1:n]]
        }))
        pb <- progress::progress_bar$new(total = length(gvect))
        entropy = apply( closest,2, function(d) {
                a = table(d)
                ret = 0
                if ( length(a) > 1 ) {
                        p = a/sum(a)
                        ret = -sum(p*log(p))
                }
                pb$tick()
                ret}
        )
        sumFunc(entropy)
} )
