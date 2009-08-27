\name{names2factors}
\Rdversion{1.1}
\alias{names2factors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Decode design factors from names
}
\description{
This function takes a vector of (e.g. file-)names that uses a fixed scheme to 
encode a factorial design and returns a data frame with the factors decoded
This is useful for filenames which encode specific data
}
\usage{
names2factors(nams, formula, sep = ".", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nams}{
a vector of names (e.g., obtained with \code{dir(pat="\\.dat"))})
}
  \item{formula}{
a formula that specifies how to decode the name (see details below)
}
  \item{sep}{
a single character that is used as a separator in the names
}
  \item{\dots}{
currently ignored
}
}
\details{
An example where this function may be used is the case where data for different subjects obtained
in different conditions are stored in seperate files, such that the file name encodes the subjects
identity and the specific condition under which the data were recorded.

For instance
	\code{S01_LDD_error.dat}
may contain the response times collected from subject 01, with stimuli presented on the Left, pointing Down, that were Difficult to recognize, on which the subject made an error.

To specify this encoding in the formula, use \code{+} wherever the seperator (\code{_} in this case) occurs, and use \code{:} to indicate which factors are encoded by a specific segment of the name. The variables in the formula are used to name the factors. It's clearer from an example: The above encoding would be specified as
	\code{~ subj + side:orientation:level + response}
The name will be split at the separator (\code{_}), yielding segments \code{"S01"}, \code{"LDD"}, \code{"error.dat"}, of which the first is used as the value for the factor \code{subj}, and the third is used as the value for the factor \code{response}. The second segment, \code{"LDD"}, is decoded by splitting it three segments one for each of the factors specified in the formula term \code{side:orientation:level} (viz., \code{side}, \code{orientation}, \code{level}). In this case the factor \code{side} would get \code{"L"} as its value, \code{orientation} would get \code{"D"}, etc.

If the name segment corresponding to a formula term with multiple factors (such as \code{side:orientation:level}) contains fewer characters than the number of factors, an error occurs. If a name segment contains more characters than the number of factors in the corresponding formula term, the letters are spread as evenly as possible, that last factor always receiving the smallest number of characters. For instance if the formula above would not be concerned with the orientation encoded, it could read
	\code{~ subj + side:level + response}
In this case, the segment \code{"LDD"} would be split into \code{"LD"} and \code{"D"} and assigned to the factors \code{side} and \code{level} respectively. 
}
\value{
A data frame with factors in columns whose names are the variable names in the formula.
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
	nams <- c("PP01_ME_Pi_LDE-error.dat", "PP02_SK_PI_RDE-correct.dat", "PP03_CB_PI_LDD-correct.dat",  "PP04_AB_DRF_LDM-error.dat")
	# could be e.g.: nams <- dir(pat=".dat")
	nams <- gsub("[-.]","_",nams)        # turn '.' and '-' into separator '_'
	names2factors(nams, ~ subj + initials + task + stim:level + resp + dummy, sep="_")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ design }% __ONLY ONE__ keyword per line
