\name{grid.tsplotAt}
\Rdversion{1.1}
\alias{grid.tsplotAt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grid.tsplotAt(ts, x, y, width, height, just = "centre", name, xlim = NA, ylim = NA, angle = 0, add = FALSE, ..., box = FALSE, axes = FALSE, ylab = "", rot = 90, fontsize = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
%%     ~~Describe \code{ts} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{just}{
%%     ~~Describe \code{just} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{angle}{
%%     ~~Describe \code{angle} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{box}{
%%     ~~Describe \code{box} here~~
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{rot}{
%%     ~~Describe \code{rot} here~~
}
  \item{fontsize}{
%%     ~~Describe \code{fontsize} here~~
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
function(ts, x, y, width, height, just="centre", name, xlim=NA, ylim=NA,
	angle = 0, add=FALSE, ..., box=FALSE, axes=FALSE, ylab = "", rot=90, fontsize=7)
{
	t = time(ts)
	if(is.na(xlim))
		xlim = range(t)
	if(is.na(ylim))
		ylim = range(ts)
	if(missing(name))
		name = "vp1"
	if(!add){
		vp0 = viewport(x,y,unit(width, "npc"),unit(height,"npc"), default.units="native",just=just)
		vp1 = dataViewport(xlim,ylim, name=name)
		pushViewport(vp0)
		pushViewport(vp1)
		if(box)
			grid.rect()
		if(axes) {
			grid.xaxis(gp=gpar(..., fontsize=fontsize))
			grid.yaxis(gp=gpar(..., fontsize=fontsize))
		}
		grid.text(ylab, x=unit(if(axes) -3 else -0.5,"lines"), rot=rot, gp=gpar(..., fontsize=fontsize))
	}
	else {
		seekViewport(name)
	}
	grid.lines(x=t, y=ts, default.units = "native", gp=gpar(...))
	upViewport(2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
