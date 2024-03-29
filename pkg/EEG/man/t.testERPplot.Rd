\name{ttestERPplot}
\Rdversion{1.1}
\alias{ttestERPplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OBSOLETE plotting function
}
\description{
OBSOLETE plotting function
}
\usage{
ttestERPplot(x, y, i = 2, alpha = 0.05, xdens = NULL, cicolx = "gray", legx = "topright", legy = ylim[2] * 0.9, type = c("between", "within"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
\code{bvadata} or \code{mean.bvadata} objects
}
  \item{i}{
channel that needs to be plotted
}
  \item{alpha}{
?
}
  \item{xdens}{
density of hatch marks confidence interval for \code{x}
}
  \item{cicolx}{
fill color confidence interval for \code{x}
}
  \item{legx, legy}{
legend x and y coordinates
}
  \item{type}{
"between" or "within" comparison in t-test?
}
}
\details{
Probably OBSOLETE or may change drastically
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
function(x,y,i=2,alpha=0.05,xdens=NULL,cicolx='gray',legx="topright", legy=ylim[2]*.9,type=c('between','within'))
{
 xm = x$mean()[,i]; xsd = x$sd()[,i]; ym = y$mean()[,i]; ysd = y$sd()[,i];
 xn = x$info()$ntrial; yn = y$info()$ntrial;
 tim0x = x$time0indx - x$segmindx;
 tim0y = y$time0indx - y$segmindx;
 timex = (x$header$`Common Infos`$SamplingInterval/1e3)*(-1+1:length(xm)-tim0x[1]); # msec
 timey = (y$header$`Common Infos`$SamplingInterval/1e3)*(-1+1:length(ym)-tim0y[1]); # msec
 if(sd(timex-timey)>1e-4)
	stop("Unequal sampling times not implemented")
 d = xm - ym;
 type = match.arg(type)
 if(type == "between"){
   g1 = xsd^2/xn;
   g2 = ysd^2/yn
   dse = sqrt(g1 + g2);
   Tbf = d / dse;
   nu = (g1+g2)^2/(g1^2/(xn-1) + g2^2/(yn-1));
   xuci = (xm+ym)/2+1.96*dse/2; xbci = (xm+ym)/2-1.96*dse/2; 
 }
 else {
   wx = x$ntrial/sum(x$ntrial); wy = y$ntrial / sum(y$ntrial)
   rawx = x$x()[,i,]*outer(rep(1,x$info()$nsamp), wx); rawy = y$x()[,i,]*outer(rep(1,y$info()$nsamp), wy)
   if(!isTRUE(all.equal(dim(rawx),dim(rawy))))
   	 stop("data are not compatible with within design")
   rawd = (rawx - rawy)/2
   xm = rowMeans(rawx)
   ym = rowMeans(rawy)
   d = rowMeans(rawd)
   ds2 = (xn/(xn-1)) * (rowMeans(rawd^2) - d^2)
   dse = sqrt(ds2/xn)
   Tbf = d / dse;
   nu = xn - 1;
   xuci = (xm+ym)/2+1.96*dse; xbci = (xm+ym)/2-1.96*dse; 
 }
 p = 1-pt(Tbf, nu)

 ylim = range(xuci,xbci,xm,ym)
 nms = if(is.character(i)) i else unique(c(dimnames(x$x())[[2]][i],dimnames(y$x())[[2]][i]))
 plot(timex, xm, type='l', col=2, lwd=3, ylim=ylim,xlab="Time",ylab=expression(Amplitude~(mu~V)), 
     main=paste(nms,collapse=" & "))
 polygon(c(timex,rev(timex)), c(xuci,rev(xbci)), col=cicolx, density=xdens);
 lines(timex, xm, col=2, lty=1, lwd=3)
 lines(timex, ym, col=3, lty=1, lwd=3)
 polygon(c(timex,rev(timex)), c(xuci,rev(xbci)));
 lines(timex, xm, col='white', lty=1, lwd=0.5)
 lines(timex, ym, col='white', lty=1, lwd=0.5)
 legend(legx,legy, c(deparse(substitute(x)),deparse(substitute(y))), col=2:3, lwd=3, lty=1)
 rug(stimes <- timex[p<alpha/2 | p>1-alpha/2])
 invisible(stimes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
