\name{print.bvadata}
\Rdversion{1.1}
\alias{print.bvadata}
\alias{print.mean.bvadata}
\alias{print.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Console display of bvadata objects
}
\description{
Print basic info about bvadata, mean.bvadata, and mbvadata.frame objects
}
\usage{
\method{print}{bvadata}(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE)
\method{print}{mean.bvadata}(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE)
\method{print}{mbvadata.frame}(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
bvadata object
}
  \item{\dots}{
further arguments
}
  \item{digits, quote, right, row.names}{
see \code{\link{print}}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
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
\code{\link{print}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, ...) {
   cat("file: ", paste("\n",x$file),"\n");
   print(unlist(x$info()))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
