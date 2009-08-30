\name{mean.bvadata}
\Rdversion{1.1}
\alias{mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Evoked/Event Related Potentials
}
\description{
Compute the average signals across trials---i.e., compute the ERPs from a bvadata object.
}
\usage{
\method{mean}{bvadata}(x, weights = 1, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
bvadata object
}
  \item{weights}{
vector with the same number of weights as there are trials; the weighted average is used if specified
}
  \item{na.rm}{
currently ignored
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{mean.bvadata} object with components
  \item{nsamp }{number of time samples for each signal}
  \item{nchan }{number of channels/signals}
  \item{ntrial }{number of trials }
  \item{sum.x }{\code{nsamp}-by-\code{nchan} matrix with signals summed across trials }
  \item{sum.x2 }{\code{nsamp}-by-\code{nchan} matrix with squared signals summed across trials }
  \item{n }{\code{nsamp}-by-\code{nchan} matrix with the number of trials on which \code{sum.x} and \code{sum.x2} are based. This can be different for each sample in each channel due to missing values or bad channels}
  \item{stimindx, sigmindx, time0indx}{mainly for compatibility with \code{\link[importBVA]{bvadata}} objects}
  \item{file}{file name on which the average is based}
  \item{header}{see \code{\link[importBVA]{bvadata}} }
  \item{mean}{\code{nsamp}-by-\code{nchan} matrix across trials signal averages (i.e., the ERPs)}
  \item{sd}{\code{nsamp}-by-\code{nchan} matrix with the across trials signal standard deviations }
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
x = gavgs[1,1]
class(x) # == 'mean.bvadata'
eeg.plot(x)
}

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
\keyword{ manip }
