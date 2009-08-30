\name{gavgs}
\Rdversion{1.1}
\alias{gavgs}
\docType{data}
\title{
Sample mbvadata frame with grand average ERPs
}
\description{
Sample mbvadata frame containing the ERP grand averages across 16 subjects in a discrimination and a detection task.
}
\usage{data(gavgs)}
\format{
  An mbvadata frame with 39 observations on the following 5 variables.
  \describe{
    \item{\code{V1}}{a collection of \code{mean.bvadata} objects}
    \item{\code{task}}{a factor with levels \code{dlf} \code{drf} \code{pi} indicating the task performed}
    \item{\code{stim}}{a factor with levels \code{cat} \code{ld} \code{rd} indicating the type of stimulus}
    \item{\code{level}}{a factor with levels \code{ch} \code{d} \code{e} \code{m} indicating the difficulty of the  stimulus}
    \item{\code{resp}}{a factor with levels \code{correct} \code{error} qualifying the response}
  }
}
\details{
The rows correspond to the \sQuote{grand average ERPs} obtain from 16 subjects participating in an experiment that involved a simple detection task and a discrimination task.

In the (\code{pi}) discrimination task subjects had to discriminate between two types of stimuli (\code{ld} and \code{rd}) which was either easy (\code{e}), medium (\code{m}), or difficult (\code{d}), by pressing one of two pressure measuring buttons mounted to their left and right thumbs. In catch trials (\code{cat}) no stimulus appeared (\code{ch}) and the subjects were not supposed to respond. The grand averages where computed across subjects. 

The simple detection task was performed in two blocks, in each of which the subject responded only with either the left or the right hand by pressing a force measuring button as soon as a stimulus appeared on the screen. The same stimuli of the different difficulty levels were used in this task. The order of left (\code{dlf}) or right (\code{dlr}) response in the first block was counter balanced across subjects, but there were missing data. The grand averages were computed across subjects and responding hand. 

}
\source{
http://users.fmg.uva.nl/rgrasman
}
\references{
paper in preparation
}
\examples{
\dontrun{
data(gavgs)

# average across detection left-first and right-first 
ngavgs = gavgs
ngavgs$task = as.factor(ifelse(gavgs$task \%in\% c("dlf","drf"), "detect", as.character(gavgs$task)))
ngavgs = aggregate(ngavgs, ngavgs[,-1], mean) # new mbvadata frame


# compare detection and discrimination tasks averaged across other factors
ngavgs.task = aggregate(ngavgs, ngavgs[,"task"], mean) # mbvadata frame with two rows
}
}
\keyword{datasets}
