context('GrabDRCs')

set.seed(100)

to = sample(colnames(cellexalObj@data), 100)

part = reduceTo( cellexalObj, what='col', to=to)

merger = GrabDRCs( cellexalObj, part, prefix='part')

for ( i in 1:3){
	 expect_equal( dim(merger@drc[[i]]), c(1654, 3))
}

for ( i in 4:6){
	 expect_equal( dim(merger@drc[[i]]), c(100, 3))
}

to2 = colnames(cellexalObj@data)[ c(1:49,150:200) ]

part2 = reduceTo( merger, what='col', to=to2)

for ( i in 1:3){
	 expect_equal( dim(part2@drc[[i]]), c(100, 3))
}

for ( i in 4:6){
	 expect_equal( dim(part2@drc[[i]]), c(5, 3))
}
