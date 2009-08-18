t.test.mbvadata.frame <-
function(obj1, obj2, chan='Cz', ylim, xlim, alpha=0.05, xdens=NULL, cicolx='gray',legx="topright", 
	legy=ylim[2]*.9, type=c('within', 'between'), cname, pat="\\..")
{
	type = match.arg(type)
	if(class(obj1)!=class(obj2)) 
		stop('not both of class %s', class(obj1))
	M = unclass(obj1)$data[[1]]$mean
	time1 = time(M)
	time2 = time(M)
	if(!all.equal(time1, time2))
		stop('Unequal times not implemented')
	nms = names(unclass(obj1)$data[[1]]$mean[1,chan])
	X1 = sapply(unclass(obj1)$data, function(x) x$mean[,chan])
	X2 = sapply(unclass(obj2)$data, function(x) x$mean[,chan])
	if(type=="within"){
		if(missing(cname)){
			nbr = max(ncol(X1), ncol(X2))
			cnam1 = gsub(pat, "", abbreviate(colnames(X1),nbr))
			cnam2 = gsub(pat, "", abbreviate(colnames(X2),nbr))
			cname = intersect(cnam1,cnam2)
			cnam1 = cnam1 %in% cname
			cnam2 = cnam2 %in% cname
			if(sum(cnam1)==0)
				stop("Could not match observations in the two data sets. Provide 'cname' or 'pat'")
		} else {
			if(is.list(cname)){
				cnam1 = cname[[1]]
				cnam2 = cname[[2]]
			}
			else stop("'cname' should be a list with two vectors of column names for matching")
		}
		X = X1[,cnam1]-X2[,cnam2]
		# n = ncol(X); m = rowMeans(X); s = sqrt(n/(n-1)) * sqrt(rowMeans(X^2) - m^2); t = sqrt(n) * m / s; p = 1 - pt(t, n-1)
		t.tests = apply(X, 1, t.test, conf.level = 1-alpha)
	}
	else {
		t.tests = sapply(seq_along(time1), function(i) t.test(X1[i,], X2[i,], conf.level = 1-alpha))
	}
	t.tests = sapply(t.tests, as.matrix)
	tval = unlist(t.tests[1,])
	df = unlist(t.tests[2,]) 
	pval = unlist(t.tests[3,])
	cis = do.call(rbind, t.tests[4,]) * qt(1-alpha, df) / qt(1-alpha/2, df)
	m = unlist(t.tests[5,])
	xm = rowMeans(X1)
	ym = rowMeans(X2)
	xuci = (xm+ym)/2 - m + cis[,2]
	xbci = (xm+ym)/2 - m + cis[,1]
	if(missing(ylim))
		ylim = range(xuci,xbci,xm,ym)
	if(missing(xlim))
		xlim = range(time1)
	ylab = unlist(unclass(obj1)$data[[1]]$header$Chan)
	ylab = strsplit(ylab[pmatch('Cz', ylab)],",")[[1]][4]
	timex = as.numeric(time1)
	plot(timex, xm, type='l', col=2, lwd=3, xlim=xlim, ylim=ylim, xlab="Time", 
		ylab=ylab, main=paste(nms,collapse=" & "))
	polygon(c(timex,rev(timex)), c(xuci,rev(xbci)), col=cicolx, density=xdens, border=NA);
	lines(timex, xm, col=2, lty=1, lwd=3)
	lines(timex, ym, col=3, lty=1, lwd=3)
#	polygon(c(timex,rev(timex)), c(xuci,rev(xbci)), border=NA);
	lines(timex, xm, col='white', lty=1, lwd=0.5)
	lines(timex, ym, col='white', lty=1, lwd=0.5)
	legend(legx,legy, c(deparse(substitute(obj1)),deparse(substitute(obj2))), col=2:3, lwd=3, lty=1)
	stimes <- timex[pval<alpha]
	rug(stimes[stimes>xlim[1] & stimes<xlim[2]])
	invisible(data.frame(time=timex, t=tval, df=df, p.value=pval, conf.int=cis, mean=m, 
		alternative=t.tests[7,1][[1]], method=t.tests[8,1][[1]]))
}

