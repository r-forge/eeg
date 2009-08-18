\name{eeg.plot}
\Rdversion{1.1}
\alias{eeg.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eeg.plot(obj, ..., xlim = NA, ylim = NA, add = FALSE, width = 0.1, height = width/2, fontsize = 7, label = TRUE, axes = FALSE, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{fontsize}{
%%     ~~Describe \code{fontsize} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
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
function(obj, ..., xlim=NA, ylim=NA, add=FALSE, width=.10, height=width/2, fontsize=7, 
	label=TRUE, axes = FALSE, na.rm=TRUE)
{
	loc = do.call('rbind',lapply(obj$header$Coor, 
				function(x) eval(parse(text=paste('c(',x,')')))))
	loc[,2:3] = loc[,3:2] * 2 * pi / 360
	chinfo = do.call('rbind', strsplit(unlist(obj$header$`Channel Infos`),",") )
	# Cartesian coordinates:
	x = loc[,1]*cos(loc[,2])*sin(loc[,3])
	y = loc[,1]*sin(loc[,2])*sin(loc[,3])
	z = loc[,1]*cos(loc[,3])
	# my projection: ?
	x = ifelse(sqrt(x^2+y^2)>0, abs(loc[,3]) * x / sqrt(x^2+y^2) / max(abs(loc[,3])), 0);
	y = ifelse(sqrt(x^2+y^2)>0, abs(loc[,3]) * y / sqrt(x^2+y^2)^(1/10) / max(abs(loc[,3])), 0);
	x[loc[,1]==0] = c(-1,  1)
	y[loc[,1]==0] = c(-1, -1)
	if(!add){
		grid.newpage()
		pushViewport(vp0 <- dataViewport(range(x)+width,range(y),exten=.15, name="head"))
	}
	else {
		seekViewport("head")
		vp0 <- current.viewport()
	}
	for(i in 1:nrow(loc)){
		chnm = chinfo[i,1]; 
		unit = chinfo[i,4]
		ylab = if(chnm \%in\% label || label==TRUE) paste(chnm," (", unit,")",sep="") else ""
		grid.tsplotAt(as.ts(obj[,i,], na.rm=na.rm), x[i], y[i], width, height, ylab=ylab, fontsize=fontsize, 
			xlim=xlim, ylim=ylim, name = chinfo[i,1], add=add, axes = chnm \%in\% axes || axes==TRUE, ...)
	}
	upViewport()
	invisible(vp0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
