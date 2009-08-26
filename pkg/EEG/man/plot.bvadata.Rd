\name{plot.bvadata}
\Rdversion{1.1}
\alias{plot.bvadata}
\alias{plot.mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot bvadata as time series
}
\description{
Simple plotting of signal averages across trials
}
\usage{
\method{plot}{bvadata}(x, ..., main = deparse(substitute(x)))
\method{plot}{mean.bvadata}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
bvadata object
}
  \item{\dots}{
extra graphical parameters; see \code{link{par}}
}
  \item{main}{
plot title
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
Only 10 channels can be plot simultaneously; see \code{\link[plot.ts]{plot}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link[plot.ts]{plot}}, \code{\link[as.ts.bvadata]{as.ts}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, ..., main=deparse(substitute(x))) {
	plot(as.ts(x, ...), ..., main=main);
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
