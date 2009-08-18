\name{mean.bvadata}
\Rdversion{1.1}
\alias{mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mean.bvadata(obj, weights = 1, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
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
function(obj, weights=1, na.rm=TRUE)
{
	dims   = dim(obj$x())
	nsamp  = dims[1]
	nchan  = dims[2]
	ntrial = dims[3]
	if(length(weights)!=1 && length(weights) != nrchan)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
