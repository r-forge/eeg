\name{[.mbvadata.frame}
\Rdversion{1.1}
\alias{[.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[.mbvadata.frame(x, rows, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{rows}{
%%     ~~Describe \code{rows} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
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
function(x, rows, cols)
{
	x = unclass(x)
	if(missing(rows))
		rows = 1:nrow(x$df)
	if(missing(cols))
		cols = 1:ncol(x$df)
	if(is.character(rows))
		rows = pmatch(rows, rownames(x$df))
	if(is.character(cols))
		cols = pmatch(cols, colnames(x$df))
	df = x$df[rows, cols, drop=FALSE]
	mbvadata = x$data[rows]
	rownames(df) = names(mbvadata)
	if(x$data.nm \%in\% colnames(x$df)[cols]) 
		structure(list(df=df, data.nm = x$data.nm, data=mbvadata), class="mbvadata.frame")
	else
		df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
