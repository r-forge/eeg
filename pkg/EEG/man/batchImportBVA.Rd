\name{batchImportBVA}
\Rdversion{1.1}
\alias{batchImportBVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch import EEG data sets exported from Brain Vision Analyzer
}
\description{
Import multiple BVA exported EEG data sets in a folder based on a filter pattern.
}
\usage{
batchImportBVA(path, pat = "vhdr$", nm.pat = "^(.+).+vhdr", newnm.pat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
path to the directory that contains the data sets to be imported
}
  \item{pat}{
\link[regex]{regular expression} that is used to filter the directory listing obtained with \code{dir()}
}
  \item{nm.pat}{
\link[regex]{regular expression} pattern that is used to extract info from the file name that is used to create a name for the data set object
}
  \item{newnm.pat}{
\link[regex]{regular expression} pattern that specifies how the info extracted with \code{nm.pat} is to be combined in the new data objects name
}
}
\details{
This is simply a wrapper function for multiple calls to \code{readBVADataFile}.
}
\value{
A list with in each component the imported data sets as \code{bvadata} objects.
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
\code{\link{readBVADataFile}}
}
\examples{
\dontrun{
dat <- batchImportBVA("./raw_ascii_eeg", pat="\\.vhdr$", nm.pat="(.+)\\.vhdr$", newnm.pat="\\1")

erps <- lapply(dat, mean) # compute ERPs

# store the ERPs in a mbvadata frame
nams <- names(erps)
nams <- gsub("-", "_", nams)
erps <- mbvadata.frame(erp=erps, names2factors(nams, ~subj+initials+task+stim:level+resp))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ classes }
