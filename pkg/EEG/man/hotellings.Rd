\name{hotellings}
\Rdversion{1.1}
\alias{hotellings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hotellings(data, mu = rep(0, NCOL(data)), print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{print}{
%%     ~~Describe \code{print} here~~
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
function(data,mu=rep(0,NCOL(data)),print=TRUE){
   cv  <- cov(data);
   mn  <- apply(data,2,mean);
   n   <- nrow(data);
   p   <- ncol(data)
   df1 <- p
   df2 <- n-p
 
   T2  <- (n / (n-1)) * t(mn-mu) \%*\% solve(cv) \%*\% (mn-mu);
   F   <- T2 * df2/df1;
 
   prb <- 1-pf(F,df1,df2);
   dd  <- t(data) - mu \%o\% rep(1,n)
   phoc  <- apply( dd, 1, t.test );
   ph.p  <- unlist(lapply(phoc,function(x) x$p.value));
   ph.t  <- unlist(lapply(phoc,function(x) x$statistic));
   ph.df <- unlist(lapply(phoc,function(x) x$parameter));
 
   if(print) cat('\nOmnibus test for null hypothesis that the mean vector = [',mu,']\n\n');

   T2 <- c('Hotellings T2 '=T2)
   if(print) print(data.frame(Value=T2, 'Exact F'=F, Hyp.df=df1, Err.df=df2, Signif.=prb));
   if(print) cat('\nPost hoc t-tests\n');

   names(ph.t) <- names(data);
   r<-data.frame(t.value=ph.t,df=ph.df,Sig.=round(ph.p,9));
   if(print) print(r)
   if(print) cat('\n');
 
   invisible(list(Hotelings.T2=T2, Exact.F=F, Hyp.df=df1, Err.df=df2, Signif.=prb, posthoc=r));
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
