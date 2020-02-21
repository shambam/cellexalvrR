#' the cellexalTime class is mainly to bring a usable order into the time mess
#' 
#' 
#' @name cellexalTime-class
#' @rdname cellexalTime-class
#' @title cellexalTime class definition
#' @description  A simple wrapper to hadle the time and time color mappings.
#' @slot dat all data needed for the time plotting and group creation
#' @slot gname the group name
#' @slot drc the drc name this object has been selected from
#' @exportClass cellexalvrR

setClass("cellexalTime", 
	slots=list(
		dat="data.frame", 
		gname="character",
		drc="character"
		)
)

#' @name plotTime
#' @aliases plotTime,cellexalTime-method
#' @rdname plotTime-methods
#' @docType methods
#' @description 3D plot the time with a line in the time
#' @param x the object
#' @title description of function plot
#' @export 
if ( ! isGeneric('plotTime') ){setGeneric('plotTime', ## Name
	function ( x ) { 
		standardGeneric('plotTime')
	}
) }

setMethod('plotTime', signature = c ('cellexalTime'),
	definition = function ( x ) {
	rgl::plot3d( x@dat[,c('a','b','c')], col= x@dat[,'col'] )
	rgl::points3d( x@dat[,c('x','y','z')], col='green')
} )


#' @name exportSelection
#' @aliases exportSelection,cellexalTime-method
#' @rdname exportSelection-methods
#' @docType methods
#' @description Export this time as selection file
#' @param x the object
#' @param fname the file to write the info to
#' @title description of function exportSelection
#' @export 
if ( ! isGeneric('exportSelection') ){setGeneric('exportSelection', ## Name
	function (x, fname) { 
		standardGeneric('exportSelection')
	}
) }

setMethod('exportSelection', signature = c ('cellexalTime'),
	definition = function (x, fname) {

	dat = x@dat[order(x@dat$time),]
	d = cbind( rownames(dat), as.vector(dat$col), rep( x@drc , nrow(dat) ), rep(0, nrow(dat))  )
	write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file= fname) 

	f2 = paste( sep=".",fname,'points')

	d = cbind( names(dat$c), dat$x, dat$y, dat$z  )
	write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file=f2)

	invisible(x)
} )


#' @name addSelection
#' @aliases addSelection,cellexalvrR-method
#' @rdname addSelection-methods
#' @docType methods
#' @description add this selection to any cellexal object
#' @param x the cellexalTime object
#' @param cellexalObj the cellexalvrR object to add the time as group
#' @param upstreamSelection which group was the basis for this timeline?
#' @title description of function addSelection
#' @export 
if ( ! isGeneric('addSelection') ){setGeneric('addSelection', ## Name
	function ( x, cellexalObj, upstreamSelection ) { 
		standardGeneric('addSelection')
	}
) }

setMethod('addSelection', signature = c ('cellexalTime', 'cellexalvrR'),
	definition = function ( x, cellexalObj, upstreamSelection ) {

		id= (ncol(cellexalObj@userGroups) /2) + 1
		x@gname = paste( "Time.group", id, sep="." ) 
		if ( is.null( cellexalObj@usedObj$timelines )){
		  cellexalObj@usedObj$timelines= list()
		}

	cellexalObj@usedObj$timelines[['lastEntry']] = x
	cellexalObj@usedObj$timelines[[ x@gname ]] = x

	## I need to create a new one named 
	info = groupingInfo(cellexalObj, upstreamSelection )
	info$selectionFile = paste( sep=".", cellexalObj@usedObj$SelectionFiles[[ upstreamSelection ]], 'time')
	info$timeObj = x



	info$gname = x@gname
	cellexalObj@groupSelectedFrom[[ x@gname ]] = info

	m = match( rownames(x@dat), colnames(cellexalObj@data) )
	if ( length(which(is.na(m)))>0){
		stop("ERROR: This timeline describes cells that are not in the ecellexalvrR object!")
	}
	cellexalObj@userGroups[,x@gname] = NA
	cellexalObj@userGroups[m,x@gname] = x@dat$time
	cellexalObj@userGroups[,paste(x@gname, sep=" ", 'order')] = NA
	cellexalObj@userGroups[m,paste(x@gname, sep=" ", 'order')] = x@dat$order
	
	cellexalObj@usedObj$lastGroup = x@gname

	invisible( cellexalObj)
} )


#' @name check
#' @aliases check,cellexalvrR-method
#' @rdname check-methods
#' @docType methods
#' @description checks for NA elements in the table and removes them
#' @param x the cellexalTime object
#' @title description of function check
#' @export 
if ( ! isGeneric('check') ){setGeneric('check', ## Name
	function (x) { 
		standardGeneric('check')
	}
) }

setMethod('check', signature = c ('cellexalTime'),
	definition = function (x) {
	bad=which( is.na(x@dat$time))
	if ( length(bad) > 0 ){
		warning("Missing values detected in the time - dropping these")
		x@dat= x@dat[-bad,]
	}
	invisible(x)
} )

#' returns a color vector based on the intern col column and the cell names
#' Not time containing cells will get gray(0.6) as color.
#'
#' @name color
#' @aliases color,cellexalvrR-method
#' @rdname color-methods
#' @docType methods
#' @description return the color in exactly the right format to color the names
#' @param x the cellexalTime object
#' @param names the cells to color with the time color
#' @title description of function color
#' @export 
if ( ! isGeneric('color') ){setGeneric('color', ## Name
	function (x, names) { 
		standardGeneric('color')
	}
) }

setMethod('color', signature = c ('cellexalTime'),
	definition = function (x, names) {
	col = rep( gray(0.6), length(names) )
	m = match( names, rownames(x@dat) )
	col[which(!is.na(m))] = as.vector(x@dat$col[m[which(!is.na(m))]])
	col
} )
