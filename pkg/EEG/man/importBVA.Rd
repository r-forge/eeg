\name{importBVA}
\Rdversion{1.1}
\alias{importBVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
importBVA(file = file.choose())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
