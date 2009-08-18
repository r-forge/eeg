\name{compERPplot}
\Rdversion{1.1}
\alias{compERPplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compERPplot(x, y, i = 2, cicolx = "gray", cicoly = "steelblue", xdens = NULL, ydens = 12, xlocleg = max(timex) * 0.7, ylocleg = ylim[2] * 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{cicolx}{
%%     ~~Describe \code{cicolx} here~~
}
  \item{cicoly}{
%%     ~~Describe \code{cicoly} here~~
}
  \item{xdens}{
%%     ~~Describe \code{xdens} here~~
}
  \item{ydens}{
%%     ~~Describe \code{ydens} here~~
}
  \item{xlocleg}{
%%     ~~Describe \code{xlocleg} here~~
}
  \item{ylocleg}{
%%     ~~Describe \code{ylocleg} here~~
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
function(x,y,i=2,cicolx="gray", cicoly="steelblue", xdens=NULL, ydens=12,
xlocleg=max(timex)*.7, ylocleg=ylim[2]*.9)
{
 xm = x$mean()[,i]; xse = x$se()[,i]; ym = y$mean()[,i]; yse = y$se()[,i];
 xuci = xm+1.96*xse; xbci = xm-1.96*xse; yuci=ym+1.96*yse; ybci=ym-1.96*yse;
 ylim = range(xuci,xbci,yuci,ybci)
 timex = (x$header$`Common Infos`$SamplingInterval/1e3)*(-1+1:length(xm)); # msec
 timey = (y$header$`Common Infos`$SamplingInterval/1e3)*(-1+1:length(ym)); # msec
 plot(timex,xm, type='l', col=2, lwd=3, ylim=ylim,xlab="Time",ylab=expression(Amplitude~(mu~V)))
 polygon(c(timex,rev(timex)), c(xuci,rev(xbci)), col=cicolx, density=xdens);
 polygon(c(timey,rev(timey)), c(yuci,rev(ybci)), col=cicoly, density=ydens,angle=365-45);
 lines(timex, xm, col=2, lty=1, lwd=3)
 lines(timey, ym, col=3, lty=1, lwd=3)
 legend(xlocleg,ylocleg, c(deparse(substitute(x)),deparse(substitute(y))), fill=c(cicolx,cicoly), col=2:3, lwd=3, lty=1, density=c(xdens,ydens))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
