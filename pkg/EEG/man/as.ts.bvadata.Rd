\name{as.ts.bvadata}
\Rdversion{1.1}
\alias{as.ts.bvadata}
\alias{as.ts.mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert bvadata or mean.bvadata object to time series object
}
\description{
Convert bvadata or mean.bvadata object to a multivariate time series object
}
\usage{
\method{as.ts}{bvadata}(x, ...)
\method{as.ts}{mean.bvadata}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{bvadata} object
}
  \item{\dots}{
Currently unused
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{mts} object with as many time series as there are channels.

If \code{x} is a \code{bvadata} object, the time series are the result from averaging across the available trials.
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

\seealso{
\code{\link[EEG:mean.bvadata]{mean}}, \code{\link{as.ts}}, \code{\link[stats:plot.ts]{plot}}
}
\examples{
\dontrun{
data(gavgs)
pp01 = gavgs[1,1]
plot(as.ts(pp01[,1:10,])) # maximum of 10 time series can be handled by plot
}

## The function is currently defined as
function(x, ...) {
	dt=getInfo(x, 'SamplingInterval')/1e3; # msec
	ts(x$mean(...), start=-(x$time0indx[1]-x$segmindx[1])*dt,deltat=dt); 
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ methods }
