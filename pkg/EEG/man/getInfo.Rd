\name{getInfo}
\Rdversion{1.1}
\alias{getInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find value of a node in a complect list tree
}
\description{
Find the value of a named node in a complex hierarchical list tree structure.
}
\usage{
getInfo(x, what)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
list tree
}
  \item{what}{
name of the node whose value has to be retrieved
}
}
\details{
Inefficient recursive implementation
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
function(x,what){if(length(x)==1) NULL else if(what \%in\% names(x)) x[[what]] else do.call("c",sapply(x,getInfo,what=what))}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

