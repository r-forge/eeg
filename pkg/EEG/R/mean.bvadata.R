mean.bvadata <-
function(x, ..., weights=1, na.rm=TRUE)
{
	obj = x # inefficient because it first copies all of x, but for generic consistentcy argument should be called 'x'
	dims   = dim(obj$x())
	nsamp  = dims[1]
	nchan  = dims[2]
	ntrial = dims[3]
	if(length(weights)!=1 && length(weights) != nchan)
		stop("length 'weights' incompatible")
	w = (!is.na(obj$x())) * if(length(weights)!=1) outer(matrix(1,nsamp,nchan), weights) else weights
	x = w * obj$x()
	dimnames(x) = dimnames(obj$x())
	sum.x  = apply(x, c(1,2), sum, na.rm = na.rm)
	sum.x2 = apply(x^2, c(1,2), sum, na.rm = na.rm)
	n = apply(w, c(1,2), sum, na.rm=na.rm)
	stimindx = obj$stimindx[1]
	segmindx = obj$segmindx[1]
	time0indx = obj$time0indx[1]
	file = obj$file
	header = obj$header
	header$`Common Infos`$DataPoints = nsamp
	dt=getInfo(obj, 'SamplingInterval')/1e3; # msec
	mean = ts(sum.x/n, , start=-(time0indx-segmindx)*dt,deltat=dt)
	sd = sqrt((n/(n-1))*(sum.x^2/n - mean^2))
	sd = ts(sd, start=-(time0indx-segmindx)*dt,deltat=dt)
	structure(list(nsamp=nsamp, nchan=nchan, ntrial=ntrial, 
		sum.x=sum.x, sum.x2=sum.x2, n=n,
		stimindx=stimindx, segmindx=segmindx, time0indx=time0indx,
		file=file, header=header, mean=mean, sd=sd), class="mean.bvadata")
}

