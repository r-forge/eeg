\name{mbvadata.frame}
\Rdversion{1.1}
\alias{mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean BVA Data Frame
}
\description{
This function creates an mbvadata.frame, which collect multiple mean.bvadata object into a single frame along with other tightly couples variables such as experimental design factors.
}
\usage{
mbvadata.frame(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
mean.bvadata objects
}
}
\details{
A \code{mbvadata.frame} is a \code{data.frame} like object that behaves similarly, but not identically.
}
\value{
\code{mbvadata.frame}
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

\seealso{
\code{\link[mean.mbvadata.frame]{mean}}
}
\examples{
\dontrun{
data(gavgs)
class(gavgs) # == 'mbvadata.frame'
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
