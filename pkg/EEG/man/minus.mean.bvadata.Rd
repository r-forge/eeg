\name{Ops.mean.bvadata}
\Rdversion{1.1}
%\alias{-.mean.bvadata}
\alias{+.mean.bvadata}
\alias{*.mean.bvadata}
\alias{/.mean.bvadata}
\alias{Ops.mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Algebra of mean bvadata objects
}
\description{
Algebra operations on mean bvadata objects
}
\usage{
%\method{-}{mean.bvadata}(a, b)
\method{+}{mean.bvadata}(a, b)
\method{*}{mean.bvadata}(a, b)
\method{/}{mean.bvadata}(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a,b}{
a number and a \code{mean.bvadata} object for \code{*}, \code{b} a scalar for \code{/}; two mean.bvadata objects for \code{+} and \code{-}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{mean.bvadata} object
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
function(a,b)
{
	if(missing(b))
		-1*a
	else
		a+(-1*b)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
\keyword{ manip }% __ONLY ONE__ keyword per line
