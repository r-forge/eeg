\name{mean.mbvadata.frame}
\Rdversion{1.1}
\alias{mean.mbvadata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grand Average Evoked/Event Related Potential
}
\description{
Computes the grand average ERP from the ERPs in an \code{mbvadata.frame}
}
\usage{
\method{mean}{mbvadata.frame}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
mbva.data frame
}
  \item{\dots}{
currently unused
}
}
\details{
Each ERP is weighted with the proportion of the number of trials on which it is based in the computation of the grand average ERP.
}
\value{
\code{\link{mean.bvadata}} object containing the grand average ERP
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
\code{\link[mean.bvadata]{mean}}
}
\examples{
\dontrun{
data(gavgs)
GAVG = mean(gavgs)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
