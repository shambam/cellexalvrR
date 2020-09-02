context('Entropy')

skip('useless')

prefix = './'

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

names <- c('LTHSC', 'LMPP', 'MPP', 'CMP', 'MEP', 'GMP', 'MPP1', 'MPP2', 'MPP3', 'STHSC', 'ESLAM', 'HSC1' )

group = rep(0, ncol( cellexalObj@data ))
for ( name in names ) {
	group[which(cellexalObj@meta.cell[,name] == 1)] = name
}

mds = as.matrix( cellexalObj@drc[[1]])

system.time({entropy = NaiveEntropySpherical( mds , group )})
# old R implementation
#   user  system elapsed                                                       
#  2.269   0.004   2.274 
# c++ table/entropy implemenation
#   user  system elapsed                                                       
#  1.164   0.008   1.173 

#expect_equal ( as.numeric(colnames(entropy)), c(0.001, 0.002, 0.004, 0.006, 0.008, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1))

entropy2D = NaiveEntropySpherical( cbind(mds[,1], mds[,2], rep(0, nrow(mds)) ) , group )

#expect_equal(entropy[1,16], entropy2D[1,16])

plot(
	log(as.numeric(colnames(entropy2D))), 
	og(entropy2D[1,]+1), 
	ylim=
		c(
			0, 
			max( log(c( entropy2D[1,], entropy[1,])+1))
		), 
	xlab='rel max distance',
	ylab="cummulative entropy",
    main= paste("2D vs 3D (red) entropy difference - euclidian spheres", nrow(mds),"cells"), 
    type='l'
)
lines(log(as.numeric(names(entropy[1,]))), log(entropy[1,]+1) , col='red')


