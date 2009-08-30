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
\method{all.equal}{bvadata}(target, current, important, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target, current}{
objects of class \code{bvadata}
}
  \item{important}{
character vector of header node names that need to be equal
}
  \item{\dots}{
further arguments (unused)
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Either \code{TRUE} (\code{NUL}L for \code{attr.all.equal}) or a vector of mode \code{"character"} describing the differences between target and current.
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
\code{\link{all.equal}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\dontrun{
data(gavgs)
all.equal(gavgs[1,1], gavgs[2,1])
}


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
