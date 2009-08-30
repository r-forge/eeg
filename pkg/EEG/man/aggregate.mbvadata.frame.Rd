\name{aggregate.mbvadata.frame}
\Rdversion{1.1}
\alias{aggregate.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Summary Statistics of Event Related Potentials Subsets
}
\description{
Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
}
\usage{
\method{aggregate}{mbvadata.frame}(x, by, FUN, ..., attach.errors = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
mbvadata frame
}
  \item{by}{
a list of grouping elements, each as long as the number of rows in \code{x}.
}
  \item{FUN}{
a function to compute the summary statistics which can be applied to \code{mbvadata.frame}s and returns a single \code{mean.bvadata} object (e.g., \code{\link[EEG:mean.mbvadata.frame]{mean}}).
}
  \item{\dots}{
further arguments passed to or used by methods.
}
  \item{attach.errors}{
the application of \code{FUN} to each subset is wrapped in a \code{\link{try}} statement. Should errors be attached as \code{errors} attribute of the returned \code{mbvadata.frame}?
}
}
\details{
\code{aggregate} creates new \code{mbvadata.frame} that contain subsets of the rows in \code{x} on the basis of the levels of the factors specified in \code{by}, and applies \code{FUN} to each of these. The resulting \code{mean.bvadata} objects are stored in the returned \code{mbvadata.frame}.
}
\value{
\code{mbvadata.frame} with 1 plus as many columns as there are factors in \code{by}. The column names are taken from \code{by} if available.
}
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
\code{\link{mbvadata.frame}}, \code{\link[base:aggregate]{aggregate}}, \code{\link{gavgs}}
}
\examples{
\dontrun{
data(gavgs)
gavgs.task.level = aggregate(gavgs, gavgs[,c('task', 'level')], mean)
}

## The function is currently defined as
function(x, by, FUN, ..., attach.errors=TRUE){
	tmp = split(x, by)
	tmp = tmp[sapply(tmp, nrow)>0]
	vnam = paste("V",1:NCOL(by),sep="")
	nams = if(is.null(names(by))) vnam else names(by)
	nams = ifelse(nams=="", vnam, nams)
	ftmp = lapply(tmp, function(x) try(FUN(x,...)))
	iserror = sapply(ftmp, class)=='try-error'
	errors = ftmp[iserror]
	ftmp = ftmp[!iserror]
	fnams = names(ftmp)
	frm = as.formula(paste("~",nams,collapse="+"))
	mbva = mbvadata.frame(ftmp, names2factors(fnams, frm))
	attr(mbva, "errors") = errors
	mbva
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ utilities }% __ONLY ONE__ keyword per line
