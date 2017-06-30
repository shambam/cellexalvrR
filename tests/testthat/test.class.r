#setClass("cellexalvr",slots=c(
#				data="matrix",
#				meta.cell="matrix",
#				meta.gene="matrix",
#				mds="list",
#				index="matrix",
#				tfs="vector",
#				usedObj = "list",
#				stats = "list"
#		)   
#)

d <- matrix(1, nrow=10, ncol=10 )

rownames(d) <- paste( 'gene', 1:10)
colnames(d) <- paste( 'sample', 1:10)

meta.cell <- cbind( 'SampleName' = colnames(d), 'grouping' = c(rep('A',5), rep('B',5)) )

meta.gene <- cbind( 'GeneID' = rownames(d), 'pathway' = c(rep('AO',5), rep('BO',5)) )

t <- cellexalvr$new( dat = d,meta.cell= meta.cell ,meta.gene = meta.gene )

expect_equal( auto_name(t), "Grouping_Nr.1" )
expect_equal( auto_name(t), "Grouping_Nr.2" )
expect_equal( auto_name(t,FALSE), "Grouping_Nr.2" )

n <- reduceTo( t, 'row', rownames(t$data)[1:5], name="row_reduced", copy=T )

expect_equal(n$name , "row_reduced" )
expect_equal(t$name , "cellexalvr" )

expect_equal(dim(t$data), c(10,10))
expect_equal(dim(n$data), c(5,10))

s <- reduceTo( n, 'col', colnames(t$data)[1:5], name="col_reduced", copy=F )

expect_equal(dim(n$data), c(5,5))
expect_equal(dim(t$data), c(10,10))

