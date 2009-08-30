\name{grid.tsplotAt}
\Rdversion{1.1}
\alias{grid.tsplotAt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot time series in a grid viewport
}
\description{
Plot time series in a grid viewport
}
\usage{
grid.tsplotAt(ts, x, y, width, height, just = "centre", name, xlim = NA, ylim = NA, angle = 0, add = FALSE, ..., box = FALSE, axes = FALSE, ylab = "", rot = 90, fontsize = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
\code{ts} object
}
  \item{x, y}{
coordinates for the centre of the viewport on the current
}
  \item{width, height}{
dimensions of the viewport in which the signal is plotted
}
  \item{just}{
justification of viewport relative to its (x,y) location. See \link[grid:viewport]{viewport}.
}
  \item{name}{
optional name of the viewport in which the time series is to be plotted. If left unspecified, a new viewport is created at the indicated location. See also \link[grid:viewport]{viewport}.
}
  \item{xlim, ylim}{
range of the x- and y-axes in the viewport (ignored if \code{name} is specified).
}
  \item{angle}{
unused?
}
  \item{add}{
should the plot be added to the existing plot?
}
  \item{\dots}{
further parameters that are pased to \code{\link[grid:gpar]{gpar}}
}
  \item{box}{
should a box be drawn around the plot?
}
  \item{axes}{
should the axes be ploted?
}
  \item{ylab}{
y-axis label
}
  \item{rot}{
orientation of the y-axis label
}
  \item{fontsize}{
font size of the y-label and axes annotation
}
}
\details{
This function is intended for internal use.

This function heavily relies on the \link[grid:grid-package]{grid} package.
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
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[grid:grid-package]{grid}}
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
\keyword{ aplot }
