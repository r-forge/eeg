\name{grandAvrg}
\Rdversion{1.1}
\alias{grandAvrg}
\alias{+.gavg.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute grand average across bvadata objects.
}
\description{
Compute grand average across bvadata objects. Add \code{gavg.bvadata} objects. These functions and class are OBSOLETE.
}
\usage{
grandAvrg(..., verbose = TRUE)
\method{+}{gavg.bvadata}(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
bvadata objects
}
  \item{verbose}{
should feedback be printed on console?
}
  \item{a, b}{
\code{gavg.bvadata} objects
}
}
\details{
OBSOLETE use \code{\link[mean.bvadata]{mean}} on \code{bvadata} objects and \code{\link{mbvadata.frame}} to bind them together for further analysis.
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
\section{Warning }{This function is OBSOLETE. DO NOT USE IT. TO BE REMOVED.}

\seealso{
\code{\link[mean.bvadata]{mean}}, \code{\link{mbvadata.frame}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(..., verbose=TRUE){
	objs = list(...)
	names(objs) = if(is.null(names(objs))) {
		as.character(match.call()[-1])[1:length(objs)]
	} else {
		nms = as.character(match.call()[-1])[1:length(objs)]
		nms[names(objs)!=""] = names(objs)[names(objs)!=""]
		nms
	}
	## select usable subset (may include all data sets)
	conformable = louter(list(objs), all.equal, important = c('DataType', 'SamplingInterval','SegmentDataPoints'))
	select = rep(TRUE, length(objs))
	if(!all(conformable)) {
		warning('bva data sets are not all conformable, selecting a most conformable group')
		select = conformable[which.max(rowSums(conformable)),]
		cat('data sets selected: ',names(objs)[select])
	}
	objs = objs[select]

	# dimension names
    channms = unique(unlist(sapply(objs,function(x) colnames(x$x()))))
	objnms = names(objs)

	## determine required bvadata class properties
	header = objs[[1]]$header
	time0indx = segmindx = stimindx = file = info = c()
	dims = unlist(objs[[1]]$info())
	names(dims) = c(names(dims)[-3],'ndsets')
	dims['nchan'] = length(channms)
	dims['ndsets'] = length(objs)
	dimnms = dimnames(objs[[1]]$x())
	dimnms[[2]] = channms
	dimnms[[3]] = objnms
	ses = means = array(NA, dim=dims, dimnames = dimnms)
	for(i in 1:length(objs)){
		chans = colnames(objs[[i]]$x())
		if(!is.null(objs[[i]]$badchannels)) ## maybe better through selection operator? i.e. bvaobj[,-3,]
			chans = chans[setdiff(chans, objs[[i]]$badchannels)]
		means[,chans,i] = objs[[i]]$mean()[,chans]
		ses[,chans,i] = objs[[i]]$se()[,chans]
		info = rbind(info, objs[[i]]$info())
		time0indx = objs[[i]]$time0indx # cbind(time0indx,objs[[i]]$time0indx)
		segmindx = objs[[i]]$segmindx # cbind(segmindx,objs[[i]]$segmindx)
		#nr0 = NROW(stimindx); nr1 = NROW(objs[[i]]$stimindx)
		#stimindx = rbind(stimindx, matrix(NA,max(0,nr1-nr0),NCOL(stimindx)))
		#stimindx = cbind(stimindx, c(objs[[i]]$stimindx,rep(NA,max(0,nr0-nr1))))
		file = c(file, objs[[i]]$file)
	}
	ntrial = unlist(info[,'ntrial'])
	y <- list(
		x = function() means,
		s = function() ses,
		acrossDatasets = function(func, samples=1:dim(means)[1], channels=1:dim(means)[2], datasets = 1:dim(means)[3], weights = 1, ...) apply((means*weights)[samples,channels,datasets, drop=FALSE],c(1,2), func, ...),
		acrossTrials = function(...) y$acrossDatasets(...), 
		mean = function(..., subset=1:dim(means)[3], weights=dim(means)[2]*outer(array(1,dim(means)[1:2]),ntrial/sum(ntrial)), na.rm=TRUE) y$acrossDatasets (mean, weights = weights, na.rm=na.rm, ...),
		sd = function(..., subset=1:dim(means)[3],   weights=dim(means)[2]*outer(array(1,dim(means)[1:2]),ntrial/sum(ntrial)), na.rm=TRUE) y$acrossDatasets (sd, weights = weights, na.rm=na.rm, ...),
		se = function(..., subset=1:dim(means)[3],   weights=dim(means)[2]*outer(array(1,dim(means)[1:2]),ntrial/sum(ntrial)), na.rm=TRUE) y$acrossDatasets (sd, weights = weights, na.rm=na.rm, ..., datasets=subset)/sqrt(dim(means)[3]),
		info = function() {inf=c(as.list(dims[-3]),list(ntrial=dim(means)[3])); attr(inf,'ndsets') ='ntrial indicates number of data sets'; attr(inf, 'bvadata.sets') = objnms; inf}, # ntrial is only used for compatibility with other functions that assume it for bvadata objects... sorry
		ntrial = ntrial,
		time0indx = time0indx,
		segmindx = segmindx,
		stimindx = stimindx,
		file = file,
		markers = objs[[1]]$markers,
		header = header
	)
	structure(y, class=c("gavg.bvadata","bvadata"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
