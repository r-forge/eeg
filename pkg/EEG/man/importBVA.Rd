\name{importBVA}
\Rdversion{1.1}
\alias{importBVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import data sets exported from Brain Vision Analyzer
}
\description{
Import data sets exported from Brain Vision Analyzer in ascii files including header and marker files.
}
\usage{
importBVA(file = file.choose())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
name of the header file ('.vhdr') to be imported
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{bvadata} object with components
\item{x }{a function that accepts no arguments and returns the raw data array}
\item{acrossTrials }{ a function that accepts a function \code{func} and arrays \code{samples},
\code{channels}, and \code{trials}. When called, the function \code{func} is applied to the selected 
channels and samples across the selected trials using \code{\link{apply}}.}
\item{mean }{a function that returns the average of the signals across trials---i.e., it computes the ERP.}
\item{sd }{a function that returns the standard deviation of the signals across trials.}
\item{se }{a function that returns the standard error of the ERPs obtained with \code{$mean}.}
\item{info }{a function that returns a vector containing the number of samples (\code{nsamp}), the number of channels (\code{nchan}), and the number of trials (\code{ntrial}) }
\item{stimindx}{a vector containing the indices of the samples on which the stimulus was presented}
\item{segmindx}{a vector containing the indices of the start of each trial epoch}
\item{time0indx}{a vector containing the indices of the samples that defines t=0 for each trial}
\item{file}{the full path to file from which the data were read}
\item{markers}{a data frame containing the marker file information}
\item{header}{a list structure that contains all information from the header file of the data set}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Raoul Grasman
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
pp01 = importBVA() # pin point to the .vhdr file of the data set in the finder window
pp01 # shows basic info on the data set
}

## The function is currently defined as
function(file=file.choose())
{
   txt = scan(file,character(), sep="\n", quiet=TRUE);
   txt = txt[-grep("^\\s*;",txt)]; # remove comments
   if(length(grep("Brain Vision Data Exchange Header File", txt))==0)
     stop("not a valid Brain Vision Data Exchange Header File");

   sects = grep("^\\[(\\w| )+\\]$", txt);
   str = list()
   length(str) = length(sects)
   names(str) = gsub("\\[|\\]","",txt[sects]);
   # parse header info into lists
   for(i in 1:(length(str)-1)){
      tmp = txt[sects[i]:(sects[i+1]-2)+1];
      str[[i]] = eval(parse(text=paste("list(",paste(gsub("=([^0-9]|.*[^0-9\\.].*)$",'="\\1"',tmp),collapse=","),")")));
   }
   tmp = txt[(sects[length(sects)]+1):length(txt)];
   str[[length(sects)]] = eval(parse(text=paste("list(",paste(gsub("=(.*[^0-9\\.].*)$",'="\\1"',tmp),collapse=","),")")));
  
   dfile = gsub("[^\\\\]+$",str$`Common Infos`$DataFile,file);
   mfile = gsub("[^\\\\]+$",str$`Common Infos`$MarkerFile,file);
   if(is.null(dfile))
      warning("no data file specified in header");
   if(is.null(mfile))
      warning("no marker file specified in header");
   x = readBVADataFile(dfile,mfile,dec=str$`ASCII Infos`$DecimalSymbol);
   x$header = str;
   x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
