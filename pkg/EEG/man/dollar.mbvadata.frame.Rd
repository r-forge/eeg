\name{$.mbvadata.frame}
\Rdversion{1.1}
\alias{[.mbvadata.frame}
\alias{$.mbvadata.frame}
\alias{$<-.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract or replace subsets of mbvadata.frames.
}
\description{
Extract or replace subsets of mbvadata.frames.
}
\usage{
\method{[}{mbvadata.frame}(x, rows, cols, drop = TRUE)
\method{$}{mbvadata.frame}(x, name)
\method{$}{mbvadata.frame}(x, name) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
mbvadata frame
}
  \item{rows}{
optional vector with indices of the samples to be selected
}
  \item{cols}{
optional vector with indices of the channels to be selected
}
  \item{drop}{
logical. When \code{FALSE}, an \code{mbvdata.frame} is always returned
}
  \item{name}{
elements to extract or replace
}
  \item{value}{
A suitable replacement value: it will be repeated a whole number of times if necessary and it may be coerced. See \code{\link[Extract.data.frame]{[.data.frame}}.
}
}
\details{
Works basically the same as for data frames.
}
\value{
If the column that specifies the mean.bvadata object names is selected only, a list of the mean.bvadata objects is returnd (not the vector with the names), unless \code{drop=FALSE} is specified as a first argument. 

If the a single element from the mean.bvadata object column is selected, the corresponding mean.bvadata object is returned.

The \code{$} returns a vector of the values in any of the columns indicated by \code{name}, except if \code{name} selects the column that contains the mean.bvadata objects, then it returns a list with these objects.

In all other cases, a mbvadata.frame is returned.
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
\code{\link{importBVA}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\dontrun{
data(gavgs)
all(gavgs[,'task'] == gavgs$task) # TRUE
}


## The function is currently defined as
function(x, name)
{
	x = unclass(x)
	idx = pmatch(name, colnames(x$df), 0)
	if(idx==0) 
		NULL 
	else if(colnames(x$df)[idx]==x$data.nm) 
		x$data 
	else x$df[,idx]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ methods }
