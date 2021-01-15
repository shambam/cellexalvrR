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
#' @slot error the error message if a catched not fatal error has occured
#' @slot geneClusters a list of gene clusters that are linked to a timeline
#' @exportClass cellexalvrR

setClass("cellexalTime", 
	slots=list(
		dat="data.frame",
		gname="character",
		drc="character",
		error="character",
		geneClusters="list",
		id="character",
		parentSelection="character"
		)
)



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
	function ( x, cellexalObj, upstreamSelection=NULL  ) { 
		standardGeneric('addSelection')
	}
) }

setMethod('addSelection', signature = c ('cellexalTime', 'cellexalvrR'),
	definition = function ( x, cellexalObj, upstreamSelection=NULL ) {


		if ( is.null(upstreamSelection)){
			stop("This is where we kill")
			upstreamSelection = groupingInfo(cellexalObj , x@parentSelection)
		}
		id= (ncol(cellexalObj@userGroups) /2) + 1
		x@gname = paste( "Time.group", id, sep="." ) 
		if ( is.null( cellexalObj@usedObj$timelines )){
			cellexalObj@usedObj$timelines= list()
		}else{
			## Is this timeline a copy of an other?
			done=0
			for ( n in names(cellexalObj@usedObj$timelines)){
				if ( cellexalObj@usedObj$timelines[[n]]@id == x@id ) {
					## this should be removed
					cellexalObj@usedObj$timelines[[n]] = x
					done=1
				}
			}
			if ( done ){
				return( invisible( cellexalObj) )
			}
		}

		cellexalObj@usedObj$timelines[['lastEntry']] = x
		cellexalObj@usedObj$timelines[[ x@gname ]] = x
		## I need to create a new one named 

		info = groupingInfo(cellexalObj, upstreamSelection )

		info$selectionFile = paste( sep=".", cellexalObj@usedObj$SelectionFiles[[ upstreamSelection ]], 'time')
		info$timeObj = x

		info$gname = x@gname
		cellexalObj@groupSelectedFrom[[ x@gname ]] = info

		m = match( rownames(x@dat), rownames(cellexalObj@drc[[x@drc]]) )

		t1 = as.matrix(x@dat[,c('a','b','c')])

		t2 = as.matrix(cellexalObj@drc[[x@drc]][m,1:3])

		colnames(t1) = colnames(t2)
		#rownames(t1) = rownames(t2)
		if ( ! isTRUE( all.equal( t1, t2) ) ){
			message("CRITICAL ERROR: drc models are not the same - check in getDifferentials and likely reducteTo or reorder.samples")
			if(interactive()) { browser() }
			stop( "The drc models are not the same!")
		}

		if ( length(which(is.na(m)))>0){
			stop("ERROR: This timeline describes cells that are not in the cellexalvrR object!")
		}
	
		m = match( rownames(x@dat), colnames(cellexalObj@data) )

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

		#rgl::plot3d( cellexalObj@drc[[info$drc]][ cellexalObj@userGroups[,paste(x@gname, sep=" ", 'order')],], col=wesanderson::wes_palette("Zissou1", nrow(cellexalObj@userGroups), type = "continuous") ) 

		invisible( cellexalObj)
} )




#' @name checkTime
#' @aliases checkTime,cellexalTime-method
#' @rdname checkTime-methods
#' @docType methods
#' @description checks for NA elements in the table and removes them
#' @param x the cellexalTime object
#' @param cellexalObj an optional cellexalvrR object - if given ONLY the drc models are checked.
#' @title description of function check
#' @export 
if ( ! isGeneric('checkTime') ){setGeneric('checkTime', ## Name
	function (x, cellexalObj) { 
		standardGeneric('checkTime')
	}
) }

setMethod('checkTime', signature = c ('cellexalTime'),
	definition = function (x, cellexalObj) {
	bad=which( is.na(x@dat$time))
	if ( length(bad) > 0 ){
		warning("Missing values detected in the time - dropping these")
		x@dat= x@dat[-bad,]
	}
	x@dat = x@dat[order(x@dat$time),]
	## onestly change the color NOW!
	ids = ceiling(seq( from=0, to=10,  length.out = nrow(x@dat) +1))
	ids = ids[-1]
	if ( length( x@id )== 0 ) {
		x@id = digest::digest( x@dat, algo="md5")
	}
	x@dat$col = factor(wesanderson::wes_palette("Zissou1", 10, type = "continuous")[ ids ], 
		levels= wesanderson::wes_palette("Zissou1", 10, type = "continuous") )
	invisible(x)
} )


setMethod('checkTime', signature = c ('cellexalTime', 'cellexalvrR'),
	definition = function (x, cellexalObj) {

		error = NULL
		#browser()
		if ( is.null(cellexalObj@drc[[x@drc]])){
			error = c( error, paste( sep="",
				"The basis drc for this timeline (", cellexalObj@drc,
				") is not part of this cellexal object" ))
		}else {
			mine = x@dat[,c('a','b','c')]
			ids = NULL
			if ( !is.null(rownames(cellexalObj@drc[[x@drc]])) ){
				ids = match(rownames(x@dat), rownames(cellexalObj@drc[[x@drc]]) )
			}else {
				ids = match(rownames(x@dat), rownames(cellexalObj@data) )
			}
			other = cellexalObj@drc[[x@drc]][ids,1:3]
			colnames(mine) = c('x', 'y','z')
			colnames(other) = c('x','y','z')
			if ( !isTRUE( all.equal( as.matrix(mine), as.matrix(other))) ) {
				error = c( error, "The drc DATA is incorrect!")
			}
		}

		if ( ! is.null(error) ) {
			message(  paste(collapse="\n\r", sep=" ", c( "checkTime",error ) ) )
			x@error = paste(collapse="\n\r", sep=" ", c( "checkTime",error ) )
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



#' @name createReport
#' @aliases createReport,cellexalTime-method
#' @rdname createReport-methods
#' @docType methods
#' @description 3D plot the time with a line in the time
#' @param x the object
#' @param cellexalObj the object to get the data from
#' @param info a cellexalvrR grouping info list
#' @param deg.genes a list of genes to create the report for
#' @param otherGeneGroupings other timeline analyzed genes to in depth descibe the differences
#' @title description of function plot
#' @export 
if ( ! isGeneric('createReport') ){setGeneric('createReport', ## Name
	function ( x, cellexalObj, info, deg.genes=NULL, otherGeneGroupings=NULL ) { 
		standardGeneric('createReport')
	}
) }

setMethod('createReport', signature = c ('cellexalTime'),
	definition = function ( x, cellexalObj, info, deg.genes=NULL, otherGeneGroupings=NULL ) {

	text = NULL
	if ( is.null(deg.genes)){
		deg.genes = cellexalObj@usedObj$deg.genes
	}
	bad= which(is.na(match(deg.genes, rownames(cellexalObj@data)) ))
	if ( length(bad) > 0) {
		bad.genes = paste( collapse=", ", deg.genes[bad] )
		deg.genes = deg.genes[-bad]
		text = paste(collapse=" ", sep=" ",
			"From the requested gene list the gene(s)", 
			bad.genes, 
			"is/are not expressed in at least 1% of the cells." 
			)
	}

	#ret = list( genes = split( names(gr), gr), ofile = ofile, pngs = pngs )
	ps = cellexalObj@usedObj$sigGeneLists$lin[[cellexalObj@usedObj$lastGroup]]
	o = order(ps[,'p.value'])
	ps = ps[o,]

	p =  apply(
		cellexalObj@data[deg.genes, order(as.vector(cellexalObj@userGroups[, info$gname ] )) ], 
		1, 
		function(x) {( x- mean(x)) / sd(x) } 
	)

	## add the plots to the log
	
	try({
		ret = simplePlotHeatmaps(cellexalObj, mat= p,  fname=file.path( cellexalObj@usedObj$sessionPath,'png', info$gname ) )
		x@geneClusters[[info$gname]] = ret$genes 
		cellexalObj = logTimeLine( cellexalObj, ps, ret$genes, 
			groupingInfo( cellexalObj,info$gname), 
			png = c( ret$ofile, ret$pngs ),
			groupingInfo( cellexalObj, info$gname ), 
			text= paste(text, ret$error, sep=" ", collapse=" ") 
		) 
	} )
				
	invisible( list( cellexalObj = cellexalObj, timeline = x) )

} )


 
#' @name createStats
#' @aliases createStats,cellexalTime-method
#' @rdname createStats-methods
#' @docType methods
#' @description calculates linear peason statistics for this timeline and the expression data in the cellexalvrR object.
#' @param x the object
#' @param cellexalObj the object to get the data from
#' @param info a cellexalvrR grouping info list
#' @param num.sig the amountof genes to return as top genes (default 250)
#' @title description of function plot
#' @export 
if ( ! isGeneric('createStats') ){setGeneric('createStats', ## Name
	function ( x, cellexalObj, info, num.sig=250 ) { 
		standardGeneric('createStats')
	}
) }

setMethod('createStats', signature = c ( 'cellexalTime' ),
	definition = function ( x, cellexalObj, info, num.sig=250 ) {
	
	#cellexalObj = addSelection( x, cellexalObj, info$gname)
	# focus on our data only
	loc = reduceTo(cellexalObj, what='col', to=rownames(x@dat) )
	# get rid of genes not expressed in at least 1% of the cells
	nCells = FastWilcoxTest::ColNotZero( Matrix::t( loc@data ) )
	OK = which( nCells / ncol(loc@data)  > .01 )
	loc = reduceTo(loc, what='row', to = rownames(loc@data)[OK]  )
	
	## run stats (pearson linear correlation directly on sparse matrix)
	ps = FastWilcoxTest::CorMatrix_N(  
		loc@data[, rownames(x@dat) ], 
		x@dat$time 
	) 
	rownames(ps) = rownames(loc@data)
	addPval = function(dat){ 
		if( dat[2] < 3){
			1.1
		}
		else { 
			2* min(pt( dat[3], dat[2]-2,lower.tail = TRUE ), 
			pt( dat[3], dat[2]-2,lower.tail = FALSE ) )
		}
	}

	ps = cbind(ps, apply( ps, 1, addPval ) )
	colnames(ps) = c('cor', 'n', 't', 'p.value')

	ps[which(is.na(ps[,1])),1] = 0
	o = order(ps[,'cor'])
	ps = ps[o,]

	n = round( num.sig / 2)
	cellexalObj@usedObj$deg.genes = c(rownames(ps)[1:n], rev(rownames(ps))[n:1] )

	if ( is.null( cellexalObj@usedObj$timelines)) {
		cellexalObj@usedObj$timelines = list()
	}

	cellexalObj = addSelection( x, cellexalObj, info)
	try({
		cellexalObj= logStatResult ( cellexalObj, method ='Linear', data=ps, col='p.value'	 )
	} )
	cellexalObj@usedObj$sigGeneLists$lin[[cellexalObj@usedObj$lastGroup]] = ps
	invisible( cellexalObj )
} )


#' @name createTime
#' @aliases createTime,cellexalTime-method
#' @rdname createTime-methods
#' @docType methods
#' @description calculate time based on the internal table a b and c columns
#' @param x the object
#' @param parentSelection the selection name this time bases on.
#' @title description of function plot
#' @export
if ( ! isGeneric('createTime') ){setGeneric('createTime', ## Name
	function ( x, parentSelection=NULL ) { 
		standardGeneric('createTime')
	}
) }

setMethod('createTime', signature = c ('cellexalTime'),
	definition = function ( x, parentSelection=NULL ) {
		if ( is.null(parentSelection) && length(x@parentSelection)==0 ){
			stop( "cellexalTime::createTime needs to know the parentSelection")
		}
		if ( !is.null( parentSelection )){
			x@parentSelection = parentSelection$gname
		}
		
		dat = cbind( x@dat$a, x@dat$b )
		colnames(dat) = c('a','b')
		if ( ! var(as.vector(x@dat$c)) == 0 ){
			dat = cbind( dat, x@dat$c )
			colnames(dat) = c('a','b','c')
		}
		mode(dat) = 'numeric'
		rownames = rownames(x@dat)
		bad= which(apply( dat,1, function(d) { all(is.na(d))}))
		if ( length(bad) > 0 ) {
			message( "pseudotimeTest3D - There are NA values in the dat matrix!")
			if ( interactive() ) {browser()}
			dat = dat[-bad,]
			rownames= rownames[-bad]
		}
		opt = optGroupCountKmeans( dat )
		group = kmeans( dat , opt )
		dist_of_centers = NULL

		if ( ncol(dat) == 2) {
			dist_of_centers = FastWilcoxTest::eDist3d( group$centers[,'a'], group$centers[,'b'], rep(0, length(group$centers[,'b'])), group$cluster[1]-1 )
		}
		else {
			dist_of_centers = FastWilcoxTest::eDist3d( group$centers[,'a'], group$centers[,'b'], group$centers[,'c'], group$cluster[1]-1 )
		}
		end = which( dist_of_centers == max(dist_of_centers))

		if ( length(which(is.na(dat))) > 0 ){
			message( "pseudotimeTest3D - There are NA values in the dat matrix!")
			if ( interactive() ) {browser()}
		}

		sling = slingshot::slingshot(dat, group$cluster, start.clus= group$cluster[1], end.clus = end  ) ## assuming that the first cell selected should also be in the first cluster...
		slingTime = slingshot::slingPseudotime( sling )

		## I am interested in the longest slope
		use = 1
		if ( ncol(slingTime) > 1){
			tmp= apply( slingTime,2, function(x){ length(which(! is.na(x))) } )
			use = which(tmp == max(tmp))
		}
		o = order( slingTime[,use])

		x@dat$time = slingTime[,use]
		x@dat$order = o
		x@dat$x = sling@curves[[use]]$s[,1]
		x@dat$y = sling@curves[[use]]$s[,2]
		if ( var(x@dat$c) == 0 ){
			x@dat$z = rep(0, nrow(slingTime))
		}
		else{
			x@dat$z = sling@curves[[use]]$s[,3]
		}
		checkTime(x)
	}
 )

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


#' returns a color vector based on the intern col column and the cell names
#' Not time containing cells will get gray(0.6) as color.
#'
#' @name HTMLtable
#' @aliases HTMLtable,cellexalTime-method
#' @rdname HTMLtable-methods
#' @docType methods
#' @description return the HTMLtable in exactly the right format
#' @param x the cellexalTime object
#' @title description of function color
#' @export 
if ( ! isGeneric('HTMLtable') ){setGeneric('HTMLtable', ## Name
	function (x, names) { 
		standardGeneric('HTMLtable')
	}
) }

setMethod('HTMLtable', signature = c ('cellexalTime'),
	definition = function (x, names) {

		cellCount = table(x@dat[,'col'])
		tableHTML = paste( sep="\n",
				"### group information table",'',
				"<p> This is information on a timeline - VR and R ids are not set at the moment<p>",
			'<table>',
			'  <tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th></tr>',

			paste(collapse="\n",
				sapply( dimnames(cellCount)[[1]], function(color){
				paste(sep="",
					'<tr><td style="background-color:', 
					color,'"',
					"></td><td>",
					color,"</td><td>",
					cellCount[match(color, dimnames(cellCount)[[1]])], "</td><td></tr>"
						)
					}))
				, '</table> ',
				"<p> Linear statistics were applied to the whole set of genes vs. the pseudotime described in the table.</p>"
		)

		tableHTML
} )

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


#' @name subsetTime
#' @aliases subsetTime,cellexalTime-method
#' @rdname subsetTime-methods
#' @docType methods
#' @description 3D plot the time with a line in the time
#' @param x the object
#' @param cells the cell names to subset the time to
#' @title description of function plot
#' @export 
if ( ! isGeneric('subsetTime') ){setGeneric('subsetTime', ## Name
	function ( x, cells ) { 
		standardGeneric('subsetTime')
	}
) }

setMethod('subsetTime', signature = c ('cellexalTime'),
	definition = function ( x, cells) {
		m = match( cells, rownames(x@dat) )
		x@dat = x@dat[m,]
		x@id = digest::digest( x@dat, algo="md5")
		invisible(x)
} )

#' To run this method efficiently it is recommended to first subset the drc the selection is based on.
#' Next it is recommended to add this drc subset to the cellexal object and use the subsetTime function to also subset the timeline.
#' This way one can assure that the timepoints can be compared across the analyses.
#' This function will run the statistics and create a report for the new subset.
#' 
#' This function is highly experimental and not meant to be used from within VR!
#' 
#' @name timeAnalysisSubset
#' @aliases timeAnalysisSubset,cellexalTime-method
#' @rdname timeAnalysisSubset-methods
#' @docType methods
#' @description create a time report for a subset of cells only
#' @param x the object
#' @param cellexalObj the cellexal object to process
#' @param deg.genes dummary over these genes of the cellexalObj@usedObj$deg.genes populated during the createStats() run
#' @title description of function plot
#' @export 
if ( ! isGeneric('timeAnalysisSubset') ){setGeneric('timeAnalysisSubset', ## Name
	function ( x, cellexalObj, deg.genes=NULL ) { 
		standardGeneric('timeAnalysisSubset')
	}
) }

setMethod('timeAnalysisSubset', signature = c ('cellexalTime'),
	definition = function ( x, cellexalObj, deg.genes=NULL ) {

		loc = reduceTo( cellexalObj,what='col', to= rownames(x@dat) )
		loc = createStats( x, loc, cellexalObj@usedObj$lastGroup )

		if ( is.null(deg.genes) ){
			deg.genes = loc@usedObj$deg.genes
		}

		cellexalObj@usedObj$deg.genes = deg.genes
		
		cellexalObj = addSelection( x, cellexalObj, x@parentSelection)
		if ( is.null(cellexalObj@usedObj$sigGeneLists$lin) ) {
			cellexalObj@usedObj$sigGeneLists$lin = list()
		}
		cellexalObj@usedObj$sigGeneLists$lin[[cellexalObj@usedObj$lastGroup]] = loc@usedObj$sigGeneLists$lin[[loc@usedObj$lastGroup]]

		ret = createReport(x, cellexalObj, groupingInfo(cellexalObj, cellexalObj@usedObj$lastGroup ) )
		x = ret$timeline
		cellexalObj = ret$cellexalObj
		cellexalObj = addSelection( x, cellexalObj, cellexalObj@usedObj$lastGroup)

		invisible( cellexalObj )

} )