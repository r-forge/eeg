\name{c.bvadata}
\Rdversion{1.1}
\alias{c.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
c.bvadata(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
