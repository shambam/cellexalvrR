#' Calculates a simple entropy value for a grouping vector and a 3D drc data.
#'
#' @name NaiveEntropySpherical
#' @aliases NaiveEntropySpherical,cellexalvrR-method
#' @rdname NaiveEntropySpherical-methods
#' @docType methods
#' @description 
#' @param x the cellexalvrR object
#' @param gvect a vector with group ids
#' @param drc the dimension reduction name to process
#' @param n  the distances to use (default= NULL; max dist /100 to maxdist/10 in 100 steps )
#' @param sumFunc function to sum up the single entropy values (default sum)
#' @title description of function NaiveEntropySpherical
#' @export 
setGeneric('NaiveEntropySpherical', ## Name
        function (x, gvect, drc, n = 20, sumFunc = sum()) { ## Argumente der generischen Funktion
                standardGeneric('NaiveEntropySpherical') ## der Aufruf von standardGeneric sorgt für das Dispatching
        }
)

setMethod('NaiveEntropySpherical', signature = c ('cellexalvrR'),
        definition = function (x, gvect, drc, n = NULL, sumFunc = sum ) {
        ## this will calculate for each entry the eucledian distance 
        ## in the 3D space to all others and get the n closest ids,
        ## checks out the groupID of the colsest cells and calculate the entropy
        NaiveEntropySpherical( x = as.matrix(x@drx[[drc]]), gvect = gvect, n = n, sumFunc=sumFunc )
} )

setMethod('NaiveEntropySpherical', signature = c ('matrix'),
        definition = function (x, gvect, drc='unused', n = NULL, sumFunc = sum ) {
        ## this will calculate for each entry the eucledian distance 
        ## in the 3D space to all others and get the n closest ids,
        ## checks out the groupID of the colsest cells and calculate the entropy
        if ( !is.function(sumFunc) ){
                stop( "sumFunc needs to be an R function" )
        }
        gvect = as.character( gvect )
        if ( is.null(n) ) {
                X = c( min(x[,1]), max(x[,1]) )
                Y = c( min(x[,2]), max(x[,2]) )
                Z = c( min(x[,3]), max(x[,3]) )
                dist = FastWilcoxTest::euclidian_distances3d( X, Y ,Z )
                n = seq( dist[2] / 1000, dist[2] / 100, dist[2] / 1000)[c(1,2,4,6,8)]
                n = c(n, seq( dist[2] / 100, dist[2] / 10, dist[2] / 100)[1:5])
                message ( paste("n set to:",paste(collapse=", ", n )) )
        }
        pb <- progress::progress_bar$new(total = length(gvect))
        closest= data.frame(lapply( 1:length(gvect), function( id ) {
                d = FastWilcoxTest::eDist3d( x[,1], x[,2],x[,3], id -1 )
#               order(d)[1:n]
                pb$tick()
                unlist( lapply( n , function( N ) {
                        OK = which(d < N)
                        if ( length(OK) == 0 ){
                                return(0)
                        }
                        entrop(  gvect[ OK ])
                } ))
        }))
        ret= NULL
        if ( length(n) > 1) {
                ret = apply(closest,1, sumFunc )
        }
        else {
             ret = sumFunc(entropy)   
        }
        names(ret) = n
        return(ret)
} )