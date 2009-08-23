\name{[.mean.bvadata}
\Rdversion{1.1}
\alias{[.mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[.mean.bvadata(x, samp = 1:x$nsamp, chan = 1:x$nchan, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{samp}{
%%     ~~Describe \code{samp} here~~
}
  \item{chan}{
%%     ~~Describe \code{chan} here~~
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
function(x, samp=1:x$nsamp, chan=1:x$nchan, ...)
{
	x$sum.x = x$sum.x[samp, chan, drop=FALSE]
	x$sum.x2 = x$sum.x2[samp, chan, drop=FALSE]
	x$mean = ts(x$mean[samp, chan, drop=FALSE], start=start(x$mean), deltat=deltat(x$mean))
	x$sd = ts(x$sd[samp, chan, drop=FALSE], start=start(x$sd), deltat=deltat(x$sd))
	x$n = x$n[samp, chan, drop=FALSE]
	x$nchan = length(chan)
	x$nsamp = length(samp)
	x$header$`Common Infos`$DataPoint = length(samp)
	x$header$`Common Infos`$NumberOfChannels = length(chan)
	chnm = dimnames(x$sum.x)[[2]]
	hchnm = unlist(x$header$`Channel Infos`)
	x$header$`Channel Infos` = x$header$`Channel Infos`[pmatch(chnm,hchnm)]
	x$header$`Coordinates` = x$header$`Coordinates`[pmatch(chnm,hchnm)]
	x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
