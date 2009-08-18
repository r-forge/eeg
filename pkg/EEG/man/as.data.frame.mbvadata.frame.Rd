\name{as.data.frame.mbvadata.frame}
\Rdversion{1.1}
\alias{as.data.frame.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
as.data.frame.mbvadata.frame(x, channels, samples, from, to, vnames, FUN = c, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{channels}{
%%     ~~Describe \code{channels} here~~
}
  \item{samples}{
%%     ~~Describe \code{samples} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{vnames}{
%%     ~~Describe \code{vnames} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
			if(!all(channels \%in\% 1:NCOL(M)))
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
