\name{mbvadata.frame}
\Rdversion{1.1}
\alias{mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mbvadata.frame(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function(...)
{
	args = list(...)
	nms = names(args)
	nms[nms==""] = paste("V",1:length(args),sep="")[nms==""]
	names(args) = nms
	mbva.data = args[[1]]
	if(!all(sapply(mbva.data,class)=="mean.bvadata"))
		stop('first argument should be mean.bvadata list')
	arg1 = list(names(mbva.data)) 
	names(arg1) = names(args)[1]
	df = do.call("data.frame", c(arg1, args[-1]))
	row.names(df) = names(mbva.data)
	structure(list(df=df, data.nm = names(arg1)[1], data = mbva.data), class="mbvadata.frame")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
