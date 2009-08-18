c.bvadata <-
function(x,y, ...){
	if(missing(y) & length(list(...))==0)
		return(x)
	if(length(list(...))>0)
		y <- c(y, ...);
	important = c('DataType','NumberOfChannels','SamplingInterval','SegmentDataPoints')
	if(!isTRUE(all.equal(x$header[[1]][important],y$header[[1]][important])) || 
	   !isTRUE(all.equal(x$header[-1], y$header[-1])))
	   stop("some 'bvadata' objects are incompattible, check their $header's")
	
	header = x$header
	mrkx = x$markers
	mrky = y$markers
	p = max(as.numeric(as.character(mrkx$position)))
	mrky$position = as.factor(as.numeric(as.character(mrky$position))+p)
	mrk = rbind(mrkx, mrky)
	stimindx = c(x$stimindx, y$stimindx+p)
	segmindx = c(x$segmindx, y$segmindx+p)
	time0indx = c(x$time0indx, y$time0indx+p)
	segmindx
	file = c(x$file, y$file)
	dimx <- dim(x$x())
	dimy <- dim(y$x())
	x <- c(x$x(), y$x())
	dim(x) =  c(dimx[1:2], dimx[3]+dimy[3])
	y <- list(
		x = function() x,
		acrossTrials = function(func,samples=1:dim(x)[1], channels=1:dim(x)[2], trials = 1:dim(x)[3]) apply(x[samples,channels,trials, drop=FALSE],c(1,2), func),
		mean = function(...) y$acrossTrials (mean, ...),
		sd = function(...) y$acrossTrials (sd, ...),
		se = function(..., trials=1:dim(x)[3]) y$acrossTrials (sd, ..., trials=trials)/sqrt(dim(x)[3]),
		info = function() list(nsamp = dim(x)[1], nchan=dim(x)[2], ntrial=dim(x)[3]),
		stimindx = stimindx,
		segmindx = segmindx,
		time0indx = time0indx,
		file = file,
		markers = mrk,
		header = header
	)
	structure(y, class = 'bvadata')
}

