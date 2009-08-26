\name{all.equal.bvadata}
\Rdversion{1.1}
\alias{all.equal.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test if two bvadata objects are equal
}
\description{
test if two bvadata objects are equivalent
}
\usage{
all.equal.bvadata(x, y, important, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}, \item{y}{
objects of class \code{bvadata}
}
  \item{important}{
character vector of header node names that need to be equal
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
function(x,y,important, ...){
	if(missing(important))
		important = c('DataType','NumberOfChannels','SamplingInterval','SegmentDataPoints')
	isTRUE(all.equal(x$header[[1]][important],y$header[[1]][important])) && 
	   isTRUE(all.equal(x$header[-1], y$header[-1]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ methods }
\keyword{ programming }
