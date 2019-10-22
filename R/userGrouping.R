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
	
	cellexalObj <- renew(cellexalObj) #function definition in file 'renew.R'
	save = FALSE
	if ( file.exists(cellidfile)){ ## file from the VR env
		## add this group into the object
		cellid <- utils::read.delim(cellidfile,header=F)

		#init local vars
		id= (ncol(cellexalObj@userGroups) /2) + 1
		gname = paste( "User.group", id, sep="." ) #the VR program dependeds on it
		n = vector( 'numeric', ncol(cellexalObj@data))
		
		#find overlap with own data
		m = match(cellid[,1], colnames(cellexalObj@data))
		if ( length(which(is.na(m))) > 0 ){
			stop( paste (
				"The grouping file has cells that are not defined in the object:", 
				paste(as.vector(cellid[which(is.na(m)),1]), collapse=", " )
					))

		}
		m = match(colnames(cellexalObj@data), cellid[,1])
		n = rep( NA, ncol(cellexalObj@data))
		n[ which(! is.na(m)) ] = cellid[m[ which(! is.na(m)) ],4] +1
		
		names(n) = colnames(cellexalObj@data)
		order = n
		order[match(cellid[,1], colnames(cellexalObj@data))] = 1:nrow(cellid)
		new =(cbind(n, order))
		colnames(new) = c( gname, paste( gname, 'order' ))

		if ( ncol(cellexalObj@data) != nrow(cellexalObj@userGroups)){
			## Just create a new one...
			id = 1
			gname = paste( "User.group", id, sep="." )
			colnames(new)[1] = gname
			cellexalObj@userGroups = data.frame( new )
		}else {
			# did we already read this file?
			t <- apply( cellexalObj@userGroups, 2, function ( x ) { ok = all.equal(as.numeric(as.vector(x)),as.numeric(as.vector(n))); if ( ok == T ) {TRUE } else { FALSE } } )
			d <- apply( cellexalObj@userGroups, 2, function ( x ) { ok = all.equal(as.numeric(as.vector(x)),as.numeric(as.vector(order))); if ( ok == T ) {TRUE } else { FALSE } } )
			ok <- which( t == T )
			if ( length(ok) == 1 & length( which(d == T)) == 1) {
				## OK use the old one
				gname = names(ok)
				id = ceiling(as.vector(ok) / 2)
			}else {
				cellexalObj@userGroups = cbind(cellexalObj@userGroups, new)
			}
		}
		
		cellexalObj@groupSelectedFrom[[gname]] = unique(as.vector(cellid[,3]))
		cellexalObj@colors[[gname]] = unique(as.vector(unique(cellid[,2])))
		savePart ( cellexalObj, 'groupSelectedFrom') #function definition in file 'integrateParts.R'
		savePart ( cellexalObj, 'colors') #function definition in file 'integrateParts.R'
		savePart ( cellexalObj, 'userGroups') #function definition in file 'integrateParts.R'
		
	}
	else if ( is.na(match(cellidfile, colnames(cellexalObj@userGroups))) ==F ) {
		## the grouping is known
		gname = cellidfile
	}else {
		stop( paste("Cannot process the request for grouping",cellidfile,"\nIt is neither a file nor a known user grouping name\n" ) )
	}
	## store the grouing name
	cellexalObj@usedObj$lastGroup = gname
	savePart ( cellexalObj, 'lastGroup') #function definition in file 'integrateParts.R'
	cellexalObj
} )
