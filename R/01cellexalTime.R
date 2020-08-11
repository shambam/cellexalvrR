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
	d = cbind( rownames(dat), as.vector(dat$col), rep( x@drc , nrow(dat) ), as.numeric( dat$col )  )
	write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file= fname) 

	f2 = paste( sep=".",fname,'points')

	d = cbind( names(dat$c), dat$x, dat$y, dat$z  )
	write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file=f2)

	invisible(x)
} )


#' @name addSelection
#' @aliases addSelection,cellexalTime-method
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

	if ( is.null( cellexalObj@usedObj$timelines) ) {
		cellexalObj@usedObj$timelines = list()
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
	## BUGFIX
	## one of the two rownames is wrong!
	
	t1 = x@dat[,c('a','b','c')]
	t2 = cellexalObj@drc[[x@drc]][m,1:3]

	colnames(t1) = colnames(t2)
	#rownames(t1) = rownames(t2)
	if ( ! all( t1 == t2) ){
		message("drc models not the same")
		browser()
		stop( "The drc models are not the same!")
	}

	if ( length(which(is.na(m)))>0){
		stop("ERROR: This timeline describes cells that are not in the cellexalvrR object!")
	}
	#m = match( colnames(cellexalObj@data), rownames(x@dat))
	#OK = which(!is.na(m))
	#all.equal( colnames(cellexalObj@data)[m], rownames(x@dat))

	cellexalObj@userGroups[,x@gname] = NA
	cellexalObj@userGroups[m,x@gname] = as.vector(x@dat$time)

	cellexalObj@userGroups[,paste(x@gname, sep=" ", 'order')] = NA
	cellexalObj@userGroups[m,paste(x@gname, sep=" ", 'order')] = order( x@dat$time )
	
	#all.equal(cellexalObj@userGroups[m,paste(x@gname, sep=" ", 'order')],  order( x@dat$time ) )
	cellexalObj@usedObj$lastGroup = x@gname
	#browser()
	col = rep('gray80', ncol(cellexalObj@data))
	col[m] = as.vector(x@dat$col)
	#all.equal( rownames(cellexalObj@drc[[info$drc]])[m], rownames(x@dat))
	#rgl::open3d()
	#rgl::plot3d( cellexalObj@drc[[info$drc]], col=col)
	#rgl::points3d (cbind( x@dat[,c('a','b')], 'c'= rep(1, nrow(x@dat)) ), col= as.vector(x@dat$col))

#	rgl::plot3d( cellexalObj@drc[[info$drc]][ cellexalObj@userGroups[,paste(x@gname, sep=" ", 'order')],], col=wesanderson::wes_palette("Zissou1", nrow(cellexalObj@userGroups), type = "continuous") ) 

	invisible( cellexalObj)
} )


#' @name checkTime
#' @aliases checkTime,cellexalTime-method
#' @rdname checkTime-methods
#' @docType methods
#' @description checks for NA elements in the table and removes them
#' @param x the cellexalTime object
#' @title description of function check
#' @export 
if ( ! isGeneric('checkTime') ){setGeneric('checkTime', ## Name
	function (x, ...) { 
		standardGeneric('checkTime')
	}
) }

setMethod('checkTime', signature = c ('cellexalTime'),
	definition = function (x) {
	bad=which( is.na(x@dat$time))
	if ( length(bad) > 0 ){
		warning("Missing values detected in the time - dropping these")
		x@dat= x@dat[-bad,]
	}
	x@dat = x@dat[order(x@dat$time),]
	## onestly change the color NOW!
	ids = ceiling(seq( from=0, to=10,  length.out = nrow(x@dat) +1))
	ids = ids[-1]
	x@dat$col = factor(wesanderson::wes_palette("Zissou1", 10, type = "continuous")[ ids ], 
		levels= wesanderson::wes_palette("Zissou1", 10, type = "continuous") )
	invisible(x)
} )


setMethod('checkTime', signature = c ('cellexalvrR'),
	definition = function (x, dataObj ) {

		error = NULL
		#browser()
		if ( is.null(x@drc[[dataObj@drc]])){
			error = c( error, paste( sep="",
				"The basis drc for this timeline (", x@drc,
				") is not part of this cellexal object" ))
		}else {
			mine = dataObj@dat[,c('a','b','c')]
			ids = NULL
			if ( !is.null(rownames(x@drc[[dataObj@drc]])) ){
				ids = match(rownames(dataObj@dat), rownames(x@drc[[dataObj@drc]]) )
			}else {
				ids = match(rownames(dataObj@dat), rownames(x@data) )
			}
			other = x@drc[[dataObj@drc]][ids,1:3]
			colnames(mine) = c('x', 'y','z')
			colnames(other) = c('x','y','z')
			if ( !isTRUE( all.equal( as.matrix(mine), as.matrix(other))) ) {
				error = c( error, "The drc DATA is incorrect!")
			}
		}

		if ( ! is.null(error) ) {
			message( paste(collapse="\n\r",c( "checkTime",error ) ) )
		}
		invisible(x)
} )

#' returns a color vector based on the intern col column and the cell names
#' Not time containing cells will get gray(0.6) as color.
#'
#' @name color
#' @aliases color,cellexalTime-method
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
