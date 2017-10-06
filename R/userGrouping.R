#' @name userGrouping
#' @aliases userGrouping,cellexalvr-method
#' @rdname userGrouping-methods
#' @docType methods
#' @description  Reads a VR grouping file and creates a user.grouping column with the information
#' @description  storing the user defined grouing for later use
#' @param cellexalObj  TEXT MISSING
#' @param cellidfile  TEXT MISSING
#' @title description of function userGrouping
#' @export 
setGeneric('userGrouping', ## Name
		function (cellexalObj, cellidfile) { ## Argumente der generischen Funktion
			standardGeneric('userGrouping') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('userGrouping', signature = c ('cellexalvr'),
		definition = function (cellexalObj, cellidfile) {
			
			cellexalObj <- renew(cellexalObj)
			if ( file.exists(cellidfile)){
				cellid <- read.delim(cellidfile,header=F)
				
				grp.vec <- as.vector(cellid[,2])
				grps <- as.vector(unique(cellid[,2]))
				
				req.graph <- unique(as.vector(cellid[,3]))
				
				id= (ncol(cellexalObj$userGroups) /2) + 1
				gname = paste( "User.group", id, sep="." ) # do not change that - the VR program depends on it!
				
				n = vector( 'numeric', ncol(cellexalObj$data))
				n[match(cellid[,1], colnames(cellexalObj$data)) ] = cellid[,2]
				n[which(n==0)] = NA
				order = n
				order[match(cellid[,1], colnames(cellexalObj$data))] = 1:nrow(cellid)
				
				#I need to record the order in the grouping, too!
				if ( id == 1 ) {
					cellexalObj$userGroups = data.frame( a = n, b = order )
					
				}else {
					## did we already have this exact grouping?
					t <- apply( cellexalObj$userGroups, 2, function ( x ) { ok = all.equal(x,n); if ( ok == T ) {TRUE } else { FALSE } } )
					d <- apply( cellexalObj$userGroups, 2, function ( x ) { ok = all.equal(x,order); if ( ok == T ) {TRUE } else { FALSE } } )
					ok <- which( t == T )
					if ( length(ok) == 1 & length( which(d == T)) == 1) {
						gname = names(ok)
						id = ceiling(as.vector(ok) / 2)
					}else {
						cellexalObj$userGroups = cbind(cellexalObj$userGroups, n, order)
					}
					
				}
				colnames(cellexalObj$userGroups)[2*id-1] = gname
				colnames(cellexalObj$userGroups)[2*id] = paste( gname, 'order' )
				
				cellexalObj$groupSelectedFrom[[gname]] = req.graph
				cellexalObj$colors[[gname]] = unique(grps)	
			}
			else if ( is.na(match(cellidfile, colnames(cellexalObj$userGroups))) ==F ) {
				gname = cellidfile
			}else {
				stop( paste("Sorry I can not process the request for grouping",cellidfile,"\nIt is neither a file nor a known user grouping name\n" ) )
			}
			
			cellexalObj$usedObj$lastGroup = gname
			
			cellexalObj
		} 
)

