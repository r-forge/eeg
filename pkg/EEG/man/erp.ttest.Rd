\name{erp.ttest}
\Rdversion{1.1}
\alias{erp.ttest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generic Event Related Potential t-test function
}
\description{
Generic function for sample wise t-tests on ERPs
}
\usage{
erp.ttest(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object
}
  \item{\dots}{
further arguments to the methods
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
NOT YET IMPLEMENTED
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
\code{\link[t.test.mbvadata.frame]{t.test}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


## The function is currently defined as
function(x, ...) NextMethod("erp.test")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
