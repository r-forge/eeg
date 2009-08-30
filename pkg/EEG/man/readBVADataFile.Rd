\name{readBVADataFile}
\Rdversion{1.1}
\alias{readBVADataFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read data from Brain Vision Analyzer data file
}
\description{
Read data from Brain Vision Analyzer data file.
}
\usage{
readBVADataFile(file = file.choose(), mfile, dec = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
file name
}
  \item{mfile}{
%%     ~~Describe \code{mfile} here~~
}
  \item{dec}{
%%     ~~Describe \code{dec} here~~
}
}
\details{
Intended for internal use
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
\note{
Intended for internal use only
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{importBVA}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(file=file.choose(),mfile=gsub("\\..+$",".VMRK",file),
  dec=".")
{
   x = as.matrix(read.table(file,TRUE),dec=dec);
   mrk = as.data.frame(readMarkerFile(mfile))
   segmRows <- stimRows <- mrk$type=='New Segment'
   segindx <- tindx <- as.numeric(as.character(mrk$position[stimRows]))
   if(any(diff(diff(tindx))!=0)){
       # check if this is due to non-trial start Stimulus markers; guess which to use
       typeNum = as.numeric(gsub("\\D","",mrk$description,perl=TRUE))
       typeTbl = table(na.omit(typeNum[stimRows]))
       guessidx = which.max(typeTbl)
       guess <- as.numeric(names(guessidx))
       if(length(typeTbl)>1)
          warning("More than one type of stimulus marker. Guessed \"",mrk$description[stimRows & typeNum==guess][1],"\"\n to be the relevant marker (certainty = ",typeTbl[guessidx]/sum(typeTbl),")\n")
       tindx <- as.numeric(as.character(mrk$position[stimRows & typeNum==guess]))
       triallen = diff(tindx)[1]
       if(any(diff(diff(tindx))!=0)){
         stop("FATAL -- Not all trials have same number of samples.");
         triallen = NA;
       }
   } else
       triallen = diff(tindx)[1]
   dm = dim(x)
   nchan = dm[2]
   nsamp = dm[1]
   channames = colnames(x)   
   ntrial = length(tindx)
   trialnms = paste('trial',1:ntrial,sep='.')
   dim(x) = c(triallen, ntrial, nchan)
   x = aperm(x, c(1,3,2))
   sampnme = paste("t",1:triallen,sep="")
   dimnames(x) = list(sampnme, channames, trialnms)
   attr(x,"markers") = mrk
   stimRows <- mrk$type=='Stimulus'
   tindx <- as.numeric(as.character(mrk$position[stimRows]))
   tim0Rows <- mrk$type=='Time 0'
   tim0indx <- as.numeric(as.character(mrk$position[tim0Rows]))
   info = list(tindx = tindx, trial_len = triallen, ntrial=ntrial, nsamp=nsamp, nchan=nchan)
   y = list(x=function() x,
     acrossTrials = function(func,samples=1:dim(x)[1], channels=1:dim(x)[2], trials = 1:dim(x)[3])
          apply(x[samples,channels,trials, drop=FALSE],c(1,2), func),
     mean = function(...) y$acrossTrials (mean, ...),
     sd = function(...) y$acrossTrials (sd, ...),
     se = function(..., trials=1:dim(x)[3]) y$acrossTrials (sd, ..., trials=trials)/sqrt(dim(x)[3]),
     info = function() list(nsamp = dim(x)[1], nchan=dim(x)[2], ntrial=dim(x)[3]),
     stimindx = tindx,
     segmindx = segindx,
     time0indx = tim0indx,
     file= file
   )
   y$markers = mrk
   class(y) = 'bvadata';
   y
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
