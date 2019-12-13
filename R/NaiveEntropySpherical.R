#' Calculates a simple entropy value for a grouping vector and a 3D drc data.
#' If 2D drc data should be analyzed add a constant third column.
#'
#' The tools calculates the max dimension of the drc plot and splits this dimension into 18 radii,
#' of a total divided by c( 2e-04, 4e-04, 6e-04, 8e-04, 1e-03, 1e-03, 2e-03, 4e-03, 6e-03, 8e-03, 1e-02, 2e-02, 3e-02 4e-02 5e-02 1e-01 2e-01 5e-01)
#' 
#' For each cell and all cells within an euclidian distance less than radius[n] the group id the Shannon entropy of the group information is calculated.
#' 
#' A two column matrix is returned with the first row containing summed up entropies and the second row containing mean selected cells per cell in the analysis.
#'
#' @name NaiveEntropySpherical
#' @aliases NaiveEntropySpherical,cellexalvrR-method
#' @rdname NaiveEntropySpherical-methods
#' @docType methods
#' @description Calculate the entropy of one grouping based on a 3D data set based on euclidian max distances (spherical selections around ech cell)
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
        gvect = as.numeric( factor( gvect ))
        if ( is.null(n) ) {
                X = c( min(x[,1]), max(x[,1]) )
                Y = c( min(x[,2]), max(x[,2]) )
                Z = c( min(x[,3]), max(x[,3]) )
                dist = FastWilcoxTest::euclidian_distances3d( X, Y ,Z )
                n = seq( dist[2] / 1000, dist[2] / 100, dist[2] / 1000)[c(1,2,4,6,8)]
                n = c(n, seq( dist[2] / 100, dist[2] / 10, dist[2] / 100)[1:5])
                n = c(n,  dist[2] / 10, dist[2] / 5,  dist[2] / 2)
                n = c( seq( dist[2] / 10000, dist[2]/1000 , dist[2] / 10000)[seq(2,10,2)], n)
                message ( paste("n set to:",paste(collapse=", ", n / dist[2] )) )
        }else {
            n = sort(n)
        }

        closest = FastWilcoxTest::SphericEntropy (  x[,1], x[,2],x[,3], gvect, n )
        
        ret = apply(closest[seq(1,nrow(closest),2),],2, sumFunc )
        names(ret) = n / dist[2]

        cells = apply(closest[seq(2,nrow(closest),2),],2, sumFunc )
        cells = cells / length(gvect)

        m = rbind(ret, cells )
        ## and add the total in one step:
        total_entr =  FastWilcoxTest::entropy( gvect )
        colnames(m) = n / dist[2]
        rownames(m) = c('total entropy', 'mean selectedCells')
        cbind( m, 1 = c( sumFunc( rep(total_entr, nrow(x) ) ), nrow(x)) )
        return(m)
} )


