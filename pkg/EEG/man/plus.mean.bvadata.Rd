\name{+.mean.bvadata}
\Rdversion{1.1}
\alias{+.mean.bvadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
+.mean.bvadata(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function(a,b)
{
	add.mbva.arr <- function(a,b)
	{
		if(class(b)=="mean.bvadata"){
			tmp <- a; a <- b; b <- tmp; tmp <- NULL
		}
		a$sum.x[] = a$sum.x[] + b[]; # gives only warning if b length not multiple of a$sum.x length
		a$sum.x2[] = a$sum.x2[] + b[] * (2*a$sum.x - b[]) / a$n[];
		a$mean[] = a$mean[] + b[];
		a
	}
	add.mbva.mbva <- function(a,b)
	{
		if(getInfo(a,'DataType')!=getInfo(b,'DataType') || getInfo(a,'SamplingInterval')!=getInfo(b,'SamplingInterval'))
			stop('incompattible DataType and/or SamplingInterval in $header')
		if(a$stimindx!=b$stimindx || a$segmindx!=b$segmindx || a$time0indx!=b$time0indx)
			stop("unequal $stimindx, $segmindx or $time0indx not implemented")
		anm = dimnames(a$sum.x)
		bnm = dimnames(b$sum.x)
		smnm = union(anm[[1]], bnm[[1]])
		chnm = union(anm[[2]], bnm[[2]])
		nsamp = length(smnm)
		nchan = length(chnm)
		sum.x = sum.x2 = n = matrix(0, nsamp, nchan, dimnames = list (smnm, chnm))
		sum.x[anm[[1]], anm[[2]]] = a$sum.x
		sum.x[bnm[[1]], bnm[[2]]] = sum.x[bnm[[1]], bnm[[2]]] + b$sum.x
		sum.x2[anm[[1]], anm[[2]]] = a$sum.x2
		sum.x2[bnm[[1]], bnm[[2]]] = sum.x2[bnm[[1]], bnm[[2]]] + b$sum.x2
		n[anm[[1]], anm[[2]]] = a$n
		n[bnm[[1]], bnm[[2]]] = n[bnm[[1]], bnm[[2]]] + b$n
		stimindx = a$stimindx[1]
		segmindx = a$segmindx[1]
		time0indx = a$time0indx[1]
		file = paste(a$file, b$file, sep="+")
		header = a$header
		header$`Common Infos`$DataPoints = nsamp
		header$`Common Infos`$NumberOfChannels = nchan
		header$`Coordinates`[chnm \%in\% anm[[2]]] = a$header$`Coordinates`[pmatch(chnm, anm[[2]],0)]
		header$`Channel Infos`[chnm \%in\% anm[[2]]] = a$header$`Channel Infos`[pmatch(chnm, anm[[2]],0)]
		header$`Coordinates`[chnm \%in\% bnm[[2]]] = b$header$`Coordinates`[pmatch(chnm, bnm[[2]],0)]
		header$`Channel Infos`[chnm \%in\% bnm[[2]]] = b$header$`Channel Infos`[pmatch(chnm, bnm[[2]],0)]
		names(header$`Channel Infos`) = names(header$`Coordinates`) = paste("Ch",1:nchan, sep="")
		dt=getInfo(a, 'SamplingInterval')/1e3; # msec
		mean = ts(sum.x/n, , start=-(time0indx-segmindx)*dt,deltat=dt)
		sd = sqrt((n/(n-1))*(sum.x^2/n - mean^2))
		sd = ts(sd, start=-(time0indx-segmindx)*dt,deltat=dt)
		ntrial = a$ntrial + b$ntrial
		structure(list(nsamp=nsamp, nchan=nchan, ntrial=ntrial, 
			sum.x=sum.x, sum.x2=sum.x2, n=n,
			stimindx=stimindx, segmindx=segmindx, time0indx=time0indx,
			file=file, header=header, mean=mean, sd=sd), class="mean.bvadata")
	}
	if(class(a)==class(b)) add.mbva.mbva(a,b) else add.mbva.arr(a,b)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
