\name{bvaMrkTrialSel}
\Rdversion{1.1}
\alias{bvaMrkTrialSel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
trial subset selection of markers data frame
}
\description{
Create a marker data frame with only selected trials retained
}
\usage{
bvaMrkTrialSel(mrk, trials)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mrk}{
marker data frame (\code{marker} slot of \code{bvadata} object)
}
  \item{trials}{
numeric vector indicating the trials to be retained
}
}
\details{
This function is intended for internal use only
}
\value{
same marker data frame with unselected trials removed
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\dontrun{
pp01 = importBVA()
bvaMrkTrialSel(pp01$marker, 1:5) # retains first 5 trials
}


## The function is currently defined as
function(mrk, trials){
	if(!is.data.frame(mrk)||NROW(mrk)==0){
		warning('empty markers data.frame')
		return(mrk)
	}
	strt = which(mrk$type == 'New Segment')
	endt = c(strt[-1]-1,NROW(mrk)) 
	rows = mapply(seq, strt, endt)
	mrk[unlist(rows[trials]),]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
