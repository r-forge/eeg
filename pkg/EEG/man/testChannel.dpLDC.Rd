\name{testChannel.dpLDC}
\Rdversion{1.1}
\alias{testChannel.dpLDC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
testChannel.dpLDC(mbva.df, Chan, times, from = 85, to = 115, M = ~task + level, X = ~level, samp = if (missing(times)) samples(unclass(mbva.df)$data[[1]], from = from, to = to) else samples(unclass(mbva.df)$data[[1]], times), plot.samples = 1:70)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mbva.df}{
%%     ~~Describe \code{mbva.df} here~~
}
  \item{Chan}{
%%     ~~Describe \code{Chan} here~~
}
  \item{times}{
%%     ~~Describe \code{times} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{samp}{
%%     ~~Describe \code{samp} here~~
}
  \item{plot.samples}{
%%     ~~Describe \code{plot.samples} here~~
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
function(mbva.df, Chan, times, from=85, to=115, 
	M=~task+level, X=~level, 
	samp=if(missing(times)) 
		samples(unclass(mbva.df)$data[[1]], from=from, to=to) 
	else samples(unclass(mbva.df)$data[[1]], times),
	plot.samples = 1:70)
{
	layout(matrix(1:6,2,byrow=TRUE))
	ps = plot.samples
	tm = time(as.ts(davg.lddc))[samp]; 
	st = t.testERPplot(davg.ldec[ps,], avg.ldec[ps,,-2], Chan, type='w',legx="bottomleft")
	abline(v=tm, col='gray')
	st = t.testERPplot(davg.ldmc[ps,], avg.ldmc[ps,,-2], Chan, type='w',legx="bottomleft")
	abline(v=tm, col='gray')
	st = t.testERPplot(davg.lddc[ps,], avg.lddc[ps,,-2], Chan, type='w',legx="bottomleft")
	abline(v=tm, col='gray')
	st = t.testERPplot(avg.ldec[ps,], avg.ldmc[ps,], Chan, type='w',legx="topright")
	abline(v=tm, col='gray')
	st = t.testERPplot(avg.lddc[ps,], avg.ldmc[ps,], Chan, type='w',legx="topright")
	abline(v=tm, col='gray')
	st = t.testERPplot(avg.lddc[ps,], avg.ldec[ps,], Chan, type='w',legx="topright"); 
	abline(v=tm, col='gray');
	layout(1); #print(st)

	df = as.data.frame(mbva.df, chan=Chan, samples=samp); print(head(df))
	# multivariate approach
	rdf = manova.reshape(df, idvar='ppn', fact=c('task','level'), paste(Chan,samp,sep=".")) #,'PO4.28')) #c('Cz.31','Cz.32'))
	cn = colnames(rdf)[-1]
	fv = sapply(cn, function(x) strsplit(x, split="\\.")[[1]])
	rownames(fv) = c('chan','samp','task','level')
	idata = as.data.frame(t(fv)); #idata
	mfit = lm(as.matrix(rdf[,-1])~1); 
	list(fit=mfit, anova=
	anova(mfit, M=M, X=X, idata=idata,test='Spherical'),times=tm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
