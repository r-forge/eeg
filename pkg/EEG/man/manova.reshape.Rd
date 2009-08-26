\name{manova.reshape}
\Rdversion{1.1}
\alias{manova.reshape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape a data frame for a MANOVA or repeated measures (M)ANOVA
}
\description{
Reshape a data frame to the proper form for a (repeated measures) (M)ANOVA with \code{lm}
}
\usage{
manova.reshape(data, idvar, factors, v.names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
the data frame
}
  \item{idvar}{
the name of the variable in \code{data} that identifies the units of observation 
(cases, subjects, plots, etc.)
}
  \item{factors}{
a vector of names of (experimental) design factors in \code{data} whos combinations of values define a measurement/variable
}
  \item{v.names}{
a vector of new names to be given to the measurements/variables. If left unspecified, the values of the \code{factors} are used to define a name
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a data frame with the data in \code{data} reorganized in columns
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
level = c('easy','medium','diff')
task  = c('discrim','detect')
resp  = c('correct','error')
subj  = 1:7
df = expand.grid(subj=subj, task=task, level=level, resp=resp)
df$mrt = round(rnorm(nrow(df), 300, 20))
manova.reshape(df, 'subj', c ('task','level','resp'))

## The function is currently defined as
function (data, idvar, factors, v.names) 
{
    data
    if (missing(v.names)) 
        v.names = setdiff(colnames(data), c(idvar, factors))
    data$`hopefuly unique` = do.call(paste, c(data[factors], 
        list(sep = ".")))
    reshape(data[, c(idvar, v.names, "hopefuly unique")], idvar = idvar, 
        timevar = "hopefuly unique", v.names = v.names, direction = "wide")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
