\name{row.names.mbvadata.frame} 

\Rdversion{1.1} 

\alias{row.names.mbvadata.frame}
\alias{row.names<-.mbvadata.frame}

%- Also NEED an '\alias' for EACH other topic documented here. 

\title{
	Get and Set Row Names for mbvadata frames
} 

\description{
mbvadata frames have a row names attribute, a character vector of length the number of rows with no duplicates nor missing values.
} 

\usage{
\method{row.names}{mbvadata.frame}(x)
\method{row.names<-}{mbvadata.frame}(x, value)
}
%- maybe also 'usage' for other objects documented here. 

\arguments{
  \item{x}{
mbvadata frame
}
  \item{\dots}{
further arguments (unused OBSOLETE)
}
} 

%\details{
%%  ~~ If necessary, more details than the description above ~~
%} 

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
 

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%} 

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, ...){class(x) = "data.frame"; NextMethod("row.names")}
} 

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ utilities }% __ONLY ONE__ keyword per line 
