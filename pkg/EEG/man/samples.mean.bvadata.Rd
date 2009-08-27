\name{samples.mean.bvadata}
\Rdversion{1.1}
\alias{timeindx}
\alias{samples}
\alias{samples.mean.bvadata}
\alias{timeindx.mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Epoch samples
}
\description{
Returns the time sample indices of the times in \code{times}, or between \code{from} and \code{to}
}
\usage{
\method{samples}{mean.bvadata}(x, times, from = start(x$mean), to = end(x$mean))
timeindx(x, start, end)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series like object or \code{mean.bvadata} object
}
  \item{times}{
vector of times for which the time sample indices need to be found
}
  \item{from, to}{
alternatively, begin time of epoch and end time of epoch for which the time samples need to be found
}
}
\details{
There's no difference between \code{samples} and \code{timeindx} except for the \code{times} alternative in \code{samples}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, times, from=start(x$mean), to=end(x$mean))
{ # returns the sample indices of the times in times or between from and to
	if(missing(times))
		if(missing(to))
			which(time(x$mean)>from)[1]
		else
			which(time(x$mean)>from & time(x$mean)<to)
	else
		apply(outer(time(x$mean),times,">="), 2, which.max )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ utilities }% __ONLY ONE__ keyword per line
