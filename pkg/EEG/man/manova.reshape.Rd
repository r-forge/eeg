\name{manova.reshape}
\Rdversion{1.1}
\alias{manova.reshape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
manova.reshape(data, idvar, factors, v.names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{idvar}{
%%     ~~Describe \code{idvar} here~~
}
  \item{factors}{
%%     ~~Describe \code{factors} here~~
}
  \item{v.names}{
%%     ~~Describe \code{v.names} here~~
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
function (data, idvar, factors, v.names) 
{
    data
    if (missing(v.names)) 
        v.names = setdiff(colnames(data), c(idvar, factors))
    data$`hopefuly unique` = do.call(paste, c(data[factors], 
        list(sep = ".")))
    reshape(data[, c(idvar, v.names, "hopefuly unique")], idvar = idvar, 
        timevar = "hopefuly unique", v.names = v.names, direction = "wide")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line