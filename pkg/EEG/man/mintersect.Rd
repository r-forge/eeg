\name{mintersect}
\Rdversion{1.1}
\alias{mintersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Intersection of multiple sets
}
\description{
This function generalizes \code{intersect} to multiple sets
}
\usage{
mintersect(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, \dots}{
vectors (of the same mode) containing a sequence of items (conceptually) with no duplicated values.
}
}
\details{
	inefficient recursive implementation
}
\value{
A vector of the same mode as the imput vectors
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
\code{\link{intersect}}, ~~~
}
\examples{
mintersect(1:5, 2:7, 3:6)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ math }