\name{batchImportBVA}
\Rdversion{1.1}
\alias{batchImportBVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
batchImportBVA(path, pat = "\\.vhdr$", nm.pat = "^(.).(\\d{2}).{4}([a-z]{2,3}).([a-z]{3}).([a-z]).+vhdr", newnm.pat = paste(paste("\\", 1:nc, sep = ""), collapse = "."))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{pat}{
%%     ~~Describe \code{pat} here~~
}
  \item{nm.pat}{
%%     ~~Describe \code{nm.pat} here~~
}
  \item{newnm.pat}{
%%     ~~Describe \code{newnm.pat} here~~
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
function(path, pat = "\\.vhdr$",
		nm.pat="^(.).(\\d{2}).{4}([a-z]{2,3}).([a-z]{3}).([a-z]).+vhdr",
		newnm.pat=paste(paste("\\",1:nc,sep=""),collapse=".")){
	nms = dir(path, pat)
	enms = dir(path, pat, full.names=TRUE)
	nc = length(strsplit(nm.pat, "\\([^\\)]+\\)")[[1]]) # number of components for new names
	nm.repl = newnm.pat
	nms = gsub(nm.pat,nm.repl,tolower(dir(path,pat)),perl=TRUE)
	names(enms) = nms
	cat(length(enms)," data sets found.\n")
	if(length(enms)>0)
		cat("Importing...","\n")
	lapply(enms, function(f){cat(f,"\n"); flush.console(); importBVA(f); })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
