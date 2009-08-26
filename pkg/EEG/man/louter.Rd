\name{louter}
\Rdversion{1.1}
\alias{louter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Outer for lists
}
\description{
This function is a replacement for \code{outer} in case both arguments that are to be crossed are lists.
}
\usage{
louter(x, y, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
lists
}
  \item{FUN}{
a function that accepts two arguments to be applied to all possible pairs of elements from 
\code{x} and \code{y}
}
  \item{\dots}{
further arguments passed to \code{FUN}
}
}
\details{
\code{outer} doesn't always work well with \code{list}s. See Examples for an artificial example.

\code{FUN} does needs not be vectorized.
}
\value{
a (generalized) matrix
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
\code{\link[base:outer]{outer}}
}
\examples{
\code{outer(as.list(letters[1:5]), as.list(1:4), function(a,b)as.formula(paste(a,"~",b)))} 
# gives an error, whereas
\code{louter(as.list(letters[1:5]), as.list(1:4), function(a,b)as.formula(paste(a,"~",b)))} 
# returns as expected

## The function is currently defined as
function(x, y, FUN, ...) {if(missing(y)) y=x else if(is.function(y)){FUN = y; y=x}; sapply(x, function(x) sapply(y, function(y) FUN(x,y,...)))} # outer doesn't work well with lists
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ array }% __ONLY ONE__ keyword per line
