\name{add.gavg.bvadata}
\Rdversion{1.1}
\alias{add.gavg.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.gavg.bvadata(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function(a,b)
{
	if(!all.equal(a,b))
		stop("data sets incompattible")
	
	x1 = a$x()
	x2 = b$x()
	anam = dimnames(x1)
	bnam = dimnames(x2)
	if(length(anam[[3]])!=length(bnam[[3]])){
		on.exit(invisible(list (a.names=anam[[3]], b.names=bnam[[3]])))
		stop("incompatible data sets")
	}
	chnm = union(anam[[2]],bnam[[2]])
	tims = union(anam[[1]],bnam[[1]])
	dnma = apply(cbind(abbreviate(anam[[3]]),anam[[3]]), 1, function(x) gsub(x[1],"",x[2]))
	dnmb = apply(cbind(abbreviate(bnam[[3]]),bnam[[3]]), 1, function(x) gsub(x[1],"",x[2]))
	dnms = abbreviate(paste(abbreviate(anam[[3]]),abbreviate(bnam[[3]]),sep=""))
	dnms = paste(dnms,dnma,".",dnmb,sep="")
	nsamp = length(tims)
	nchan = length(chnm)
	ndset = length(bnam[[3]]) 
	x = array(NA,c(nsamp, nchan, ndset), dimnames=list (tims, chnm, dnms)) 
	x[anam[[1]], anam[[2]], ] = x1;
	x[bnam[[1]], bnam[[2]], ] = x[bnam[[1]], bnam[[2]], ] + x2; 
	#s1 = a$s()
	n1 = a$ntrial
	#s1 = outer(array(1,dim(x1)[-3]), n1*(n1-1)) * s1^2
	#s2 = b$s()
	n2 = b$ntrial
	#s2 = outer(array(1,dim(x2)[-3]), n2*(n2-1)) * s2^2
	#s = array(NA,dim(x), dimnames=dimnames(x))
	#s[anam[[1]], anam[[2]], ] = s1;
	#s[bnam[[1]], bnam[[2]], ] = s[bnam[[1]], bnam[[2]], ] + s2;
	#s = (s/outer(array(1,dim(s)[-3]),(n1+n2)*(n1+n2-1)))^0.5 # sacrifice bias for consistency when a + b + c + d + ... is computed
	ntrial = n1+n2
	dims = unlist(a$info())
	names(dims) = c(names(dims)[-3],'ndsets')
	dims['nchan'] = nchan
	dims['ndsets'] = ndset
	dims['nsamp'] = nsamp
	objnms = paste(anam[[3]],bnam[[3]],sep="+")
	y <- list(
		x = function() x,
		s = function() s,
		acrossDatasets = function(func, samples=1:dim(x)[1], channels=1:dim(x)[2], datasets = 1:dim(x)[3], weights = 1, ...) apply((x*weights)[samples,channels,datasets, drop=FALSE],c(1,2), func, ...),
		acrossTrials = function(...) y$acrossDatasets(...), 
		mean = function(..., subset=1:dim(x)[3], weights=dim(x)[2]*outer(array(1,dim(x)[1:2]),ntrial/sum(ntrial)), na.rm=TRUE) y$acrossDatasets (mean, weights = weights, na.rm=na.rm, ...),
		sd = function(..., subset=1:dim(x)[3],   weights=dim(x)[2]*outer(array(1,dim(x)[1:2]),ntrial/sum(ntrial)), na.rm=TRUE) y$acrossDatasets (sd, weights = weights, na.rm=na.rm, ...),
		se = function(..., subset=1:dim(x)[3],   weights=dim(x)[2]*outer(array(1,dim(x)[1:2]),ntrial/sum(ntrial)), na.rm=TRUE) y$acrossDatasets (sd, weights = weights, na.rm=na.rm, ..., datasets=subset)/sqrt(dim(x)[3]),
		info = function() {inf=c(as.list(dims[-3]),list(ntrial=dim(x)[3])); attr(inf,'ndsets') ='ntrial indicates number of data sets'; attr(inf, 'bvadata.sets') = objnms; inf}, # ntrial is only used for compatibility with other functions that assume it for bvadata objects... sorry
		ntrial = ntrial,
		time0indx = a$time0indx,
		segmindx = a$segmindx,
		stimindx = a$stimindx,
		file = paste(a$file, b$file, sep="+"),
		markers = a$markers,
		header = a$header
	)
	class = class(a)
	x1 = x2 = s1 = s2 = a = b = NULL; # free memory
	structure(y, class=class)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
