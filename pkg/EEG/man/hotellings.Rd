\name{hotellings}
\Rdversion{1.1}
\alias{hotellings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hotelling's T2 test on mean vector of multivariate data
}
\description{
Hotelling's T2 test on mean vector of multivariate data.
}
\usage{
hotellings(data, mu = rep(0, NCOL(data)), print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data frame or matrix with all numeric variables
}
  \item{mu}{
value of the mean vector according to the null hypothesis
}
  \item{print}{
should the result be printed?
}
}
\details{
Hotelling's T2 test for a single group mean vector, and posthoc t-test on individual variables.

The p-values are not corrected for multiple comparisons.
}
\value{
List with components
\item{Hotelings.T2 }{Value of the statistic}
\item{Exact.F }{F-value corresponding to the statistic}
\item{Hyp.df }{Hypothesis degrees of freedom}
\item{Err.df }{Error degrees of freedom}
\item{Signif. }{P-value}
\item{posthoc }{Table of posthoc t-tests.}
}
\references{
Stevens (1996). Applied Multivariate Statistics for the Social Sciences.
Morrison, D. F. (1978). Multivariate Statistical Methods.
}
\author{
Raoul Grasman
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{This function is better not used for regular analysis. 
The Hotelling's T2 is correct, but there are better procedures for the posthoc analysis.}

\seealso{
\code{\link{manova.reshape}}
}
\examples{
data = matrix(rnorm(5*30),,5)
hotellings(data)


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
\keyword{ utilities }
\keyword{ htest }
\keyword{ multivariate }% __ONLY ONE__ keyword per line
