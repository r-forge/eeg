\name{readMarkerFile}
\Rdversion{1.1}
\alias{readMarkerFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readMarkerFile(file, what = 3, keep.names = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
}
  \item{keep.names}{
%%     ~~Describe \code{keep.names} here~~
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
function(file, what=3, keep.names = FALSE)
{
   mrk = readLines(file)
   mrk = mrk[-(1:grep("\\[Marker Infos\\]",mrk))]
   mrk = mrk[-grep("^;",mrk)]
   mrk
   mrkans = t(sapply(mrk, function(x){y=strsplit(x, split="[,=]")[[1]];y[7]=y[7];y})) 
   if(!keep.names) {
     rownames(mrkans) = mrkans[,1];
     mrkans = mrkans[,-1];
     colnames(mrkans) = c("type", "description", "position", "size", "channel", "date")
   }
   mrkans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line