\name{[.bvadata}
\Rdversion{1.1}
\alias{[.bvadata}
\alias{[.mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract subsets of bvadata
}
\description{
Extract trials, channels and/or samples from a bvadata/mean.bvadata object
}
\usage{
\method{[}{bvadata}(x, sample = NA, channel = NA, trial = NA)
\method{[}{mean.bvadata}(x, samp = 1:x$nsamp, chan = 1:x$nchan, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
bvadata object
}
  \item{sample, samp}{
optional vector with indices of the samples to be selected
}
  \item{channel, chan}{
optional vector with indices of the channels to be selected
}
  \item{trial}{
optional vector with indices of the trials to be selected
}
  \item{...}{
unused (?)
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
Raoul Grasman
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{importBVA}}, ~~~
}
\examples{
\dontrun{
pp01 = importBVA()
pp01.subset = pp01[,,-(1:10)] # remove first 10 trials
}

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, sample=NA,channel=NA,trial=NA)
{
   inf = x$info()
   if(missing(sample))
       sample = 1:inf$nsamp;
   if(missing(channel))
       channel = 1:inf$nchan;
   if(missing(trial))
       trial = 1:inf$ntrial;
   dimnms = dimnames(x$x())
   if(!all(sample \%in\% dimnms[[1]] || abs(sample) \%in\% c(seq(dimnms[[1]]),0)) ||
      !all(channel \%in\% dimnms[[2]] || abs(channel) \%in\% c(seq(dimnms[[2]]),0)) ||
      !all(trial \%in\% dimnms[[3]] || abs(trial) \%in\% c(seq(dimnms[[3]]),0))
    )
       stop("subscript out of bounds")
   file = x$file
   header = x$header
   orig = y = x
   x = x$x()[sample, channel, trial, drop=FALSE];
   y$x=function() x;
   y$acrossTrials = function(func,samples=1:dim(x)[1], channels=1:dim(x)[2], trials = 1:dim(x)[3], ...)
          apply(x[samples,channels,trials, drop=FALSE],c(1,2), func, ...);
   y$mean = function(...) y$acrossTrials (mean, ...); 
   y$sd = function(...) y$acrossTrials (sd, ...); 
   y$se = function(..., trials=1:dim(x)[3]) y$acrossTrials (sd, ..., trials=trials)/sqrt(dim(x)[3]);
   y$info = function() list(nsamp = dim(x)[1], nchan=dim(x)[2], ntrial=dim(x)[3]);
   y$file= file
   y$markers = bvaMrkTrialSel(y$markers, trial)
   y$header = header;
   y$header$`Common Infos`$NumberOfChannels = dim(x)[2]
   y$header$`Common Infos`$DataPoints = dim(x)[1] * dim(x)[3]
   y$header$`Common Infos`$SegmentDataPoints = dim(x)[1]
   y$header$`Channel Infos` = y$header$`Channel Infos`[channel]
   y$header$Coordinates = y$header$Coordinates[channel]
   if(!is.null(y$ntrial))
   		y$ntrial = y$ntrial[trial]
   class(y) = class(orig);
   orig = NULL
   y
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ methods }
