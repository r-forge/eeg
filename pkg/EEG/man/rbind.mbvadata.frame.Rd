\name{rbind.mbvadata.frame}
\Rdversion{1.1}
\alias{rbind.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bind mbvadata frames
}
\description{
Bind the rows of two \code{mbvadata.frame} objects into a single \code{mbvadata.frame}
}
\usage{
rbind.mbvadata.frame(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a, b}{
Two \code{mbvadata.frame}s
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
an \code{mbvadata.frame}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

\dontrun{
data(gavgs)
a = gavgs[1:10,]
b = gavgs[-(1:10),]
ab = rbind(a, b)
}

## The function is currently defined as
function(a, b)
{
	if(class(a)!=class(b))
		stop("can't bind mbvadata.frame with different structure")
	a = unclass(a)
	b = unclass(b)
	df = rbind(a$df, b$df)
	mbvadata = c(a$data, b$data)
	if(!is.null(dim(df)))
		rownames(df) = names(mbvadata)
	structure(list(df=df, data.nm=a$data.nm, data=mbvadata), class="mbvadata.frame")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
