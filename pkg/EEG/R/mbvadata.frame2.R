


mbvadata.frame <- 
function(...)
{
	args = list(...)
	nms = names(unlist(args[-1], recursive = FALSE))
	nms = c(if(!is.null(names(args)[1])) names(args)[1] else "", nms)
	nms[nms==""] = paste("V",1:length(args),sep="")[nms==""]
	args = c(args[1], unlist(args[-1], recursive=FALSE))
	names(args) = nms
	mbva.data = args[[1]]
	if(!all(sapply(mbva.data,class)=="mean.bvadata"))
		stop('first argument should be mean.bvadata list')
	arg1 = list(names(mbva.data)) 
	names(arg1) = names(args)[1]
	df = do.call("data.frame", c(arg1, args[-1]))
	row.names(df) = names(mbva.data)
	attr(df, "data.nm") = names(arg1)[1]
	attr(df, "data"   ) = mbva.data
	class(df) = c("mbvadata.frame","data.frame")
	df
}

`[.mbvadata.frame` <-
function(x, rows, cols, drop = TRUE)
{
	class(x) <- "data.frame"
	if(missing(rows))
		rows = 1:nrow(x)
	if(missing(cols))
		cols = 1:ncol(x)
	if(is.character(rows))
		rows = pmatch(rows, rownames(x))
	if(is.character(cols))
		cols = pmatch(cols, colnames(x))
	df = x[rows, cols, drop=drop]
	mbvadata = attr(x, "data")[rows]
	data.nm = attr(x, "data.nm")
	if(length(rows)==1 && length(cols)==1 && colnames(x)[cols]==data.nm)
		mbvadata[[1]]
	else if(attr(x,"data.nm") %in% colnames(x)[cols])
		if(length(cols)==1 && drop)
			mbvadata
		else
			structure(df,data=mbvadata, data.nm=data.nm, class=c("mbvadata.frame", "data.frame"))
	else
		df
}


`$.mbvadata.frame` <-
function(x, name)
{
	class(x) = "data.frame"
	idx = pmatch(name, colnames(x), 0)
	if(idx==0) 
		NULL 
	else if(colnames(x)[idx]==attr(x,"data.nm")) 
		attr(x, "data") 
	else x[[idx]]
}

`$<-.mbvadata.frame` <-
function(x, name, value)
{
	class(x) = "data.frame"
	idx = pmatch(name, colnames(x),0)
	if(colnames(x)[idx]==attr(x, "data.nm")){
		if(nrow(x) %% length(value)!=0)
			warning("number of items to replace is not a multiple of replacement length")
		attr(x, "data") = rep(value, length=nrow(x))
		rownames(x) = make.unique(rep(names(value), length=nrow(x)))
		x[,idx] = rep(names(value), length=nrow(x))
	} else x[,idx] = value
	structure(x, class=c("mbvadata.frame","data.frame"))
}


as.data.frame.mbvadata.frame <-
function(x, channels, samples, from, to, vnames, FUN=c, ...) # dots for compatibility, FUN can be, e.g., colMeans to average across samples
{
	#x = unclass(x)
	data = attr(x,"data")
	df = x
	class(df) = "data.frame"
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


mean.mbvadata.frame <-
function(x, ...)
{
	class(x)  = "data.frame"
	data = attr(x, "data")
	z = data[[1]]
	for(i in 2:length(data)) z = z + data[[i]]
	z / length(x)
}

print.mbvadata.frame <- function(x, ...) { class(x) <- "data.frame"; NextMethod("print");}
row.names.mbvadata.frame <- function(x){class(x) = "data.frame"; NextMethod("row.names")}
`row.names<-.mbvadata.frame` <- function(x, value){ class(x) = "data.frame"; NextMethod("row.names");}

print.mbvadata.frame <-
function (x, ..., digits = NULL, quote = FALSE, right = TRUE, 
    row.names = TRUE) 
{
    n <- length(row.names(x))
    if (length(x) == 0L) {
        cat(gettextf("data frame with 0 columns and %d rows\n", 
            n))
    }
    else if (n == 0L) {
        print.default(names(x), quote = FALSE)
        cat(gettext("<0 rows> (or 0-length row.names)\n"))
    }
    else {
        m <- as.matrix(format.data.frame(x, digits = digits, 
            na.encode = FALSE))
        if (!isTRUE(row.names)) 
            dimnames(m)[[1L]] <- if (identical(row.names, FALSE)) 
                rep.int("", n)
            else row.names
        print(m, ..., quote = quote, right = right)
    }
    invisible(x)
}


rbind.mbvadata.frame <-
function(a, b)
{
	if(class(a)!=class(b) || is.null(attr(b,"data.nm")))
		stop("can't bind mbvadata.frame with different object")
	if(attr(a,"data.nm") != attr(b, "data.nm"))
		stop("differing names; can't match mbvadata.frames")
	class(a) = "data.frame"
	class(b) = "data.frame"
	df = rbind(a, b)
	mbvadata = c(attr(a,"data"), attr(b,"data"))
	if(!is.null(dim(df)))
		rownames(df) = make.unique(names(mbvadata))
	structure(df, data.nm=attr(a,"data.nm"), data=mbvadata, class="mbvadata.frame")
}


