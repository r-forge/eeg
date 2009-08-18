testChannel.dpLDC <-
function(mbva.df, Chan, times, from=85, to=115, 
	M=~task+level, X=~level, 
	samp=if(missing(times)) 
		samples(unclass(mbva.df)$data[[1]], from=from, to=to) 
	else samples(unclass(mbva.df)$data[[1]], times),
	plot.samples = 1:70)
{
	layout(matrix(1:6,2,byrow=TRUE))
	ps = plot.samples
	tm = time(as.ts(davg.lddc))[samp]; 
	st = t.testERPplot(davg.ldec[ps,], avg.ldec[ps,,-2], Chan, type='w',legx="bottomleft")
	abline(v=tm, col='gray')
	st = t.testERPplot(davg.ldmc[ps,], avg.ldmc[ps,,-2], Chan, type='w',legx="bottomleft")
	abline(v=tm, col='gray')
	st = t.testERPplot(davg.lddc[ps,], avg.lddc[ps,,-2], Chan, type='w',legx="bottomleft")
	abline(v=tm, col='gray')
	st = t.testERPplot(avg.ldec[ps,], avg.ldmc[ps,], Chan, type='w',legx="topright")
	abline(v=tm, col='gray')
	st = t.testERPplot(avg.lddc[ps,], avg.ldmc[ps,], Chan, type='w',legx="topright")
	abline(v=tm, col='gray')
	st = t.testERPplot(avg.lddc[ps,], avg.ldec[ps,], Chan, type='w',legx="topright"); 
	abline(v=tm, col='gray');
	layout(1); #print(st)

	df = as.data.frame(mbva.df, chan=Chan, samples=samp); print(head(df))
	# multivariate approach
	rdf = manova.reshape(df, idvar='ppn', fact=c('task','level'), paste(Chan,samp,sep=".")) #,'PO4.28')) #c('Cz.31','Cz.32'))
	cn = colnames(rdf)[-1]
	fv = sapply(cn, function(x) strsplit(x, split="\\.")[[1]])
	rownames(fv) = c('chan','samp','task','level')
	idata = as.data.frame(t(fv)); #idata
	mfit = lm(as.matrix(rdf[,-1])~1); 
	list(fit=mfit, anova=
	anova(mfit, M=M, X=X, idata=idata,test='Spherical'),times=tm)
}

