\name{eeg.plot}
\Rdversion{1.1}
\alias{eeg.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Topographically arranged ERP traces plot
}
\description{
Plots ERP traces in a topographically arrangment
}
\usage{
eeg.plot(obj, ..., xlim = NA, ylim = NA, add = FALSE, width = 0.1, height = width/2, 
	fontsize = 7, label = TRUE, axes = FALSE, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
a \code{bvadata} object or a \code{mean.bvadata} object
}
  \item{\dots}{
graphical parameters that are passed to \code{\link{grid.tsplotAt}}
}
  \item{xlim, ylim}{
ranges of the x- and y-axis for each ERP trace
}
  \item{add}{
should the plot be added to the existing plot?
}
  \item{width, height}{
dimension of each ERP graph viewport 
}
  \item{fontsize}{
font size used in graph annotation
}
  \item{label}{
should each ERP trace be labeled with the electrode name?
}
  \item{axes}{
logical, numeric vector or character vector. If \code{TRUE} the axes are plotted for each trace. If a vector the entries are used to decide for which of the channels the axes are plotted.
}
  \item{na.rm}{
should \code{NA}'s be removed before averaging? NOT YET IMPLEMENTED
}
}
\details{
If \code{obj} is a \code{bvadata} object, the mean ERP is computed across trials before plotting.
}
\value{
the outer viewport on which the traces were plotted is returned
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
\code{\link{compERPplot}}, \code{\link[t.test.mbvadata.frame]{t.test}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
\dontrun{
pp01 = importBVA()
eeg.plot(pp01[,,1:30], fontsize=5, axes=c('Cz','Pz','F_re_force'))
eeg.plot(pp01[,,31:60], col='red', add=TRUE)
grid.text("First vs. Second Half of Trials",y=unit(0.95,"npc")) # add a title

}

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
\keyword{ hplot }
\keyword{ aplot }