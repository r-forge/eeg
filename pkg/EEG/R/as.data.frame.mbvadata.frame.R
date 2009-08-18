as.data.frame.mbvadata.frame <-
function(x, channels, samples, from, to, vnames, FUN=c, ...) # dots for compatibility, FUN can be, e.g., colMeans to average across samples
{
	x = unclass(x)
	df = x$df
	data = x$data
	nc = sapply(data, function(x) NCOL(data$mean))
	M = data[[which.max(nc)]]$mean
	if(missing(channels) && missing(samples) && missing(from) && missing(from)) { # (ignore specified 'names' and 'FUN')
		return(df); 
	}
	else {
		if(missing(channels))
			channels = colnames(M)
		else if(is.numeric(channels)){
			if(!all(channels %in% 1:NCOL(M)))
				stop("channels out of bound")
			channels = colnames(M)[channels]
		}
		missing.from = missing(from)
		if(missing.from)
			from <- start(M)
#		if(missing(to))
#			if(missing.from)
#				to <- end(M)
#			else
#				to <- from + deltat(M) # one sample please
		if(missing(samples))
			if(missing(to))
				samples = which(time(M) >= from)[1]
			else
				samples = which(time(M) >= from & time(M) <= to) # include bounds most natural from user perspective?

		.samp = as.character(samples)
		.x = array(NA, c(length(samples), length(channels)), dimnames = list(.samp,channels))
		func = function(x){
				chn = intersect(channels, colnames(x$mean))
				.x[] = NA; 
				.x[.samp, chn] = x$mean[samples, chn, drop=FALSE]
				FUN(.x)
			}
		r = sapply(data, func)
		r = if(is.matrix(r)) t(r) else as.matrix(r)
		if(missing(vnames)){
			M = M[samples, channels, drop=FALSE]
			rownames(M) = samples
			vnames = names(FUN(M))
			if(is.null(vnames))
				vnames = c(t(outer(colnames(M),rownames(M),paste,sep=".")))
		}
		if(NCOL(r) == length(vnames) || NCOL(r)==NCOL(M) || NCOL(r)==NROW(M))
			colnames(r) = c(vnames)
		cbind(df, r)
	}
}

