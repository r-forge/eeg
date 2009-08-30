\name{as.data.frame.mbvadata.frame}
\Rdversion{1.1}
\alias{as.data.frame.mbvadata.frame}
\alias{data.frame.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert mbvadata.frame to data.frame with amplitude values as columns
}
\description{
Convert mbvadata.frame to a regular data.frame extracting amplitudes values at specified sets of electrodes and time samples. Useful for statistical analysis of the extracted amplitude values.
}
\usage{
\method{as.data.frame}{mbvadata.frame}(x, channels, samples, from, to, vnames, FUN = c, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
mbvadata.frame
}
  \item{channels}{
Channels for which amplitude samples have to be extracted
}
  \item{samples}{
Samples at which amplitudes have to be extracted; alternatively, specify \code{from} and \code{to}
}
  \item{from}{
Start time (in time units, e.g., msec) for epoch from which the amplitudes have to be extracted
}
  \item{to}{
End time (in time units) for epoch from which the amplitudes have to be extracted
}
  \item{vnames}{
character vector of names under which the amplitude values of different samples and channels are stored
}
  \item{FUN}{
function that can be applied to the extracted epoch/samples, e.g. \code{colMeans} for average amplitude; should return one value per channel (see details)
}
  \item{\dots}{
further arguments that are passed to \code{FUN}
}
}
\details{
The \code{mbvadata.frame} \code{x} is converted to a \code{data.frame} with the same factors as \code{x} itself. Variables are added however which are named by combining the channel name(s) and sample number(s) by default, or are given a name from \code{vnames}. The new variables are the amplitudes at the indicated channel and time sample (for each channel/sample combination a variable is added), or are the result returned by \code{FUN} (if specified.

\code{FUN} must accept a matrix and return a vector.

}
\value{
a data frame
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Raoul Grasman
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\dontrun{
data(gavgs)
as.data.frame(gavgs, from=100, to=180, chan='Cz', FUN=colMeans)
}


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
\keyword{ manip }
\keyword{ methods }
