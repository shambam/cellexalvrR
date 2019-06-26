#' VR function that ready the VR grouping information and stores it in the R object.
#' @name userGrouping
#' @aliases userGrouping,cellexalvrR-method
#' @rdname userGrouping-methods
#' @docType methods
#' @description  Reads a VR cell selection file and creates a user.grouping column with the information
#' @description  storing the user defined grouping for later use
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param cellidfile  TEXT MISSING
#' @keywords userGrouping
#' @title description of function userGrouping
#' @export userGrouping
if ( ! isGeneric('userGrouping') ){setGeneric('userGrouping', ## Name
	function (cellexalObj, cellidfile) { 
		standardGeneric('userGrouping') 
	}
) }

setMethod('userGrouping', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cellidfile) {
	
	cellexalObj <- renew(cellexalObj)
	save = FALSE
	if ( file.exists(cellidfile)){ ## file from the VR env
		## add this group into the object
		cellid <- utils::read.delim(cellidfile,header=F)
	
		grp.vec <- as.vector(cellid[,2])
		grps <- as.vector(unique(cellid[,2]))
	
		req.graph <- unique(as.vector(cellid[,3]))
	
		#add two lines into the object # one for the group and the other for the order in the group
		id= (ncol(cellexalObj@userGroups) /2) + 1
		gname = paste( "User.group", id, sep="." ) #the VR program depended on it
		n = vector( 'numeric', ncol(cellexalObj@data))
		m = match(cellid[,1], colnames(cellexalObj@data))
		if ( length(which(is.na(m))) > 0 ){
			stop( paste (
				"The grouping file has cells that are not defined in the object:", 
				paste(as.vector(cellid[which(is.na(m)),1]), collapse=", " )
					))

		}
		n[match(cellid[,1], colnames(cellexalObj@data)) ] = cellid[,2]
		n[which(n==0)] = NA
		order = n
		order[match(cellid[,1], colnames(cellexalObj@data))] = 1:nrow(cellid)
	
		#I need to record the order in the grouping
		if ( id == 1 ) { 
			## new grouping data
			cellexalObj@userGroups = data.frame( a = n, b = order )
		
		}else {
			## did we already have this exact grouping?
			t <- apply( cellexalObj@userGroups, 2, function ( x ) { ok = all.equal(as.numeric(as.vector(x)),n); if ( ok == T ) {TRUE } else { FALSE } } )
			d <- apply( cellexalObj@userGroups, 2, function ( x ) { ok = all.equal(as.numeric(as.vector(x)),order); if ( ok == T ) {TRUE } else { FALSE } } )
			ok <- which( t == T )
			if ( length(ok) == 1 & length( which(d == T)) == 1) {
				## OK use the old one
				gname = names(ok)
				id = ceiling(as.vector(ok) / 2)
			}else {
				## add this new one
				cellexalObj@userGroups = cbind(cellexalObj@userGroups, n, order)
			}
		}
		colnames(cellexalObj@userGroups)[2*id-1] = gname
		colnames(cellexalObj@userGroups)[2*id] = paste( gname, 'order' )
		
		cellexalObj@groupSelectedFrom[[gname]] = req.graph
		cellexalObj@colors[[gname]] = unique(grps)
		savePart ( cellexalObj, 'groupSelectedFrom')
		savePart ( cellexalObj, 'colors')
		savePart ( cellexalObj, 'userGroups')
		
	}
	else if ( is.na(match(cellidfile, colnames(cellexalObj@userGroups))) ==F ) {
		## the grouping is known
		gname = cellidfile
	}else {
		stop( paste("Cannot process the request for grouping",cellidfile,"\nIt is neither a file nor a known user grouping name\n" ) )
	}
	## store the grouing name
	cellexalObj@usedObj$lastGroup = gname
	savePart ( cellexalObj, 'lastGroup')
	cellexalObj
} )
