\name{compERPplot}
\Rdversion{1.1}
\alias{compERPplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot two ERP's for comparison
}
\description{
Plot single channel ERPs from two bvadata objects superimposed for comparison.
}
\usage{
compERPplot(x, y, i = 2, cicolx = "gray", cicoly = "steelblue", xdens = NULL, ydens = 12, xlocleg = max(timex) * 0.7, ylocleg = ylim[2] * 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
first bvadata object
}
  \item{y}{
second bvadata object
}
  \item{i}{
channel to be plotted
}
  \item{cicolx}{
color for the confidence interval of the ERP from bvadata object \code{x}
}
  \item{cicoly}{
color for the confidence interval of the ERP from bvadata object \code{y}
}
  \item{xdens}{
line density of \code{x}'s confidence interval (see \code{\link{polygon}})
}
  \item{ydens}{
line density of \code{y}'s confidence interval (see \code{\link{polygon}})
}
  \item{xlocleg}{
x-coordinate of the legend
}
  \item{ylocleg}{
y-coordinate of the legend
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
		x = readBVADataFile("dataset1.vhdr")
		y = readBVADataFile("dataset2.vhdr")
		compERPplot(x, y)
		compERPplot(x[,,1:30], x[,,31:60]) # compare first 30 with last 30 trials
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
